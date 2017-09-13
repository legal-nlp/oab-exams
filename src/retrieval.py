import copy
import csv
import os
import collections
from lxml import etree
import nltk
import numpy
import networkx
nltk.download('punkt')


"""
## examples

# parse OAB exam, return generator of OABQuestion instances
oab = parse_xml('/home/bruno/git/oab-exams/OAB/raw/2010-01.xml')
questions = questions_in_tree(oab)
first_q = next(questions)

# parse law XML, return list of tuples article ID and raw article text
lei = parse_xml('/home/bruno/git/oab-exams/lexml/lei-8906.xml')
artigos =  articles_in_tree(lei)

# create an instance of collection of articles, which processes the 
# text in each article, creates a node for each, creates a graph of 
# them, and caches their TF-IDF vectors
artcol = ArticleCollection(artigos, rm_stopwords=True)

# add first question to graph constructed from the articles in artcol
# return the shortest path and distance from the question statement
# to each item
paths_dict = question_paths_in_graph(artcol, first_q)

# shallow question answer justified questions in justify.txt, using
# laws in lexml/ and getting the questions at OAB/raw/
result = sqa_justified_questions('doc/justify.txt', 'lexml/', 'OAB/raw/', rm_stopwords=True) 


"""


#
## reading XML
def parse_xml(path, parser=etree.XMLParser(remove_blank_text=True)):
    return etree.parse(path)

def elements_in_tree(tree_root, element_tag):
    for element in tree_root.getiterator(tag=element_tag):
        yield element


#
## reading OAB exams
def get_exam_id(tree):
    exam_id = tree.getroot()
    return exam_id.get('year')+'-'+exam_id.get('edition')

def get_statement_text(question):
    return question.find('statement').text

def get_items(question):
    return question.find('items').getchildren()

def get_correct_item(question):
    for i in get_items(question):
        if i.get('correct') == "true":
            return i.get('letter')

def make_items_dict(items):
    return dict((i.get('letter'), getattr(i, 'text')) for i in items)

class OABQuestion():
    def __init__(self, number, exam, valid, statement, items, justification=None):
        self.number = number
        self.exam = exam
        self.valid = valid
        self.statement = statement
        self.items = items
        self.justification = justification

    def __repr__(self):
        if self.valid:
            return "OAB:{}-Q:{}-ans:{} | just: {}".format(self.exam, self.number, self.valid, self.justification)

def questions_in_tree(tree_root):
    for question in elements_in_tree(tree_root, 'question'):
        yield OABQuestion(question.get('number'),
                          get_exam_id(tree_root),
                           get_correct_item(question),
                               get_statement_text(question),
                               make_items_dict(get_items(question)))


#
## reading law XML

# lexML namespaces
nsm = {"xlink": "http://www.w3.org/1999/xlink",
           'xsi': 'http://www.w3.org/2001/XMLSchema-instance',
           "xml": "http://www.w3.org/XML/1998/namespace",
           None: "http://www.lexml.gov.br/1.0"}

def namespace_it(namespaces, key, element):
    # namespaced element in {namespace}element syntax
    return "{{{}}}{}".format(namespaces[key], element)

def lazy_articles_in_tree(tree_root, namespace=nsm):
    for artigo in elements_in_tree(tree_root, namespace_it(namespace, None, 'Artigo')):
        yield artigo.get('id'), ''.join(artigo.itertext())

def articles_in_tree(tree_root, namespace=nsm):
    return list(lazy_articles_in_tree(tree_root, namespace))

def get_urn(law_xml):
    assert isinstance(law_xml, etree._ElementTree)
    id_element = law_xml.find('Metadado/Identificacao', namespaces=nsm)
    return id_element.get('URN')

def law_articles(law_path, namespace=nsm):
    law_xml = parse_xml(law_path)
    return (law_urn, articles_in_tree(law_xml, namespace=nsm))

#
## text processing
  
def is_number(token):
    try:
        float(token.replace(',', '.').replace('º', ''))
    except ValueError:
        return False
    return True

def is_punctuation(token):
    if token in '!"#$%&\'()*+,-./:;<=>?@[\\]^_`´{|}~§–':
        return True

def is_stopword(token, language='portuguese'):
    if token in nltk.corpus.stopwords.words(language):
        return True

def is_useful(token, rm_stopwords):
    token = token.strip()
    if is_number(token) or is_punctuation(token) or (rm_stopwords and is_stopword(token)):
        return False
    else:
        return True

def preprocess_text(text, rm_stopwords):
    assert isinstance(rm_stopwords, bool)
    return [token.lower().strip() for token in nltk.tokenize.word_tokenize(text) if is_useful(token, rm_stopwords)]


#
## tf-idf and base graph making

class ArticleCollection(nltk.TextCollection):
    def __init__(self, source, rm_stopwords):
        self.rm_stopwords = rm_stopwords
        # map article index to its original number in law
        self.ids = {artigo[0]:ix for ix, artigo in enumerate(source)}
        # so that we have useful methods such as .idf(token)
        nltk.TextCollection.__init__(self, list(map(lambda x: preprocess_text(x[1], self.rm_stopwords), source)))
        # index tokens to create TF-IDF vector
        self.token_index_dict = {key:ix for ix, key in enumerate(self.vocab().keys())}
        self.vocab_size = len(self.vocab().keys())
        self.tfidf_vectors = [self.tfidf_vectorize(text) for text in self._texts]
        self.size = len(self._texts)
        # graph w/ only the articles as nodes, no edges
        self.base_graph = self.make_base_graph()

    def tf_tokens(self, tokens):
        count = collections.Counter(tokens)
        length = len(tokens)
        return list(map(lambda x: count[x]/length, tokens))

    def tfidf_vectorize(self, text):
        # text must be preprocessed first!
        tfidf_vector = numpy.zeros(self.vocab_size)
        tf_vector = self.tf_tokens(text)
        for ix, token in enumerate(text):
            idf = self.idf(token)
            if idf == 0:
                continue
            tfidf_vector[self.token_index_dict[token]] = tf_vector[ix] * idf
        return tfidf_vector

    def inverse_similarity(self, vec1, vec2):
        similarity = cosine_similarity(vec1, vec2)
        if similarity == 0:
            return numpy.Infinity
        else:
            return 1 / similarity

    def make_base_graph(self):
        graph = networkx.Graph()
        graph.add_nodes_from(self.ids.keys())
        return graph


def cosine_similarity(vec1, vec2):
    denominator = numpy.linalg.norm(vec1) * numpy.linalg.norm(vec2)
    if denominator == 0:
        return 0
    else:
        return numpy.dot(vec1, vec2) / denominator


#
## add questions

def add_temporary_node(graph, article_collection, text, label):
    """
    article_collection is where graph and tfidf-calculation happen,
    text is raw question statement (which is preprocessed here) and
    label is question number in str.
    """
    graph.add_node(label)
    label_tfidf = article_collection.tfidf_vectorize(preprocess_text(text, article_collection.rm_stopwords))
    # to add edges only to the articles, and not every node
    for node_id in article_collection.ids.keys():
        node_ix = article_collection.ids[node_id]
        graph.add_edge(label, node_id, weight=article_collection.inverse_similarity(label_tfidf, article_collection.tfidf_vectors[node_ix]))
    return graph

def question_paths_in_graph(article_collection, oab_question):
    """
    return distance and shortest path from statement to each item in
    oab_question.
    note that '1' (str) means question one and.
    """
    assert isinstance(article_collection, ArticleCollection)
    assert isinstance(oab_question, OABQuestion)
    # so that base_graph is not changed improperly:
    graph = copy.deepcopy(article_collection.base_graph)
    # add question statement:
    graph = add_temporary_node(graph, article_collection, oab_question.statement, oab_question.number)
    paths = {}
    for question_letter, item_text in oab_question.items.items():
        graph = add_temporary_node(graph, article_collection, item_text, question_letter)
        paths[question_letter] = networkx.algorithms.shortest_paths.bidirectional_dijkstra(graph, oab_question.number, question_letter, weight='weight')
    return paths

#
## add justified questions

def read_laws_into_artcollection(laws_path, rm_stopwords=False, namespaces=nsm):
    # reads all .xml files in laws_path to a dictionary of urn:artcol
    assert os.path.isdir(laws_path)
    laws = {}
    for file in os.scandir(laws_path):
        if file.name.endswith(".xml"):
            law_xml = parse_xml(file.path)
            urn = get_urn(law_xml)
            artigos = articles_in_tree(law_xml)
            artcol = ArticleCollection(artigos, rm_stopwords)
            laws[urn] = artcol
    return laws

def find_question(oab_exam, question_nr):
    assert isinstance(oab_exam, etree._ElementTree)
    for question in questions_in_tree(oab_exam):
        if question.number == question_nr:
            return question

def sqa_justified_questions(justification_path, laws_path, exams_path, rm_stopwords=False, namespace=nsm):
    # sqa = shallow question answering
    # justification file must be in the format described in docs.
    assert os.path.isfile(justification_path)
    assert os.path.isdir(exams_path)
    laws = read_laws_into_artcollection(laws_path, rm_stopwords, namespace)
    question_paths = {}
    with open(justification_path, 'r') as tsv:
        tsv = csv.reader(tsv, delimiter='\t')
        for row in tsv:
            # row[0]: OAB exam filename
            exam_path = os.path.join(exams_path, row[0] + '.xml')
            oab_exam = parse_xml(exam_path)
            # row[1]: question number
            question = find_question(oab_exam, row[1])
            # row[3]: justification law URN
            law = laws[row[3]]
            # row[2]: justification article
            question.justification = (row[3], row[2])
            paths = question_paths_in_graph(law, question)
            question_paths[question] = paths
        return question_paths

def check_justification_correct_items(question_paths):
    correct_items = {}
    for question, item_paths in question_paths.items():
        correct_letter = question.valid
        correct_item_path = item_paths[correct_letter]
        selected_article = correct_item_path[1][1]
        correct_items[question] = (selected_article == question.justification[1])
    return correct_items
