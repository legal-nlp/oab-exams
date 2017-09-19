import copy
import csv
import os
import json
from functools import reduce
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

# parse law XML, return tuple (article-ID, list-of-raw-article-text)
lei = law_articles_in_file('/home/bruno/git/oab-exams/lexml/lei-8906.xml')
leis = all_law_articles_in_path('/home/bruno/git/oab-exams/lexml/')

# create an instance of collection of articles, which processes the 
# text in each article, creates a node for each, creates a graph of 
# them, and caches their TF-IDF vectors
artcol = ArticleCollection(leis, rm_stopwords=True)
laws = read_laws_into_artcollection('/home/bruno/git/oab-exams/lexml/', False, True) # see code for arguments

# add first question to graph constructed from the articles in artcol
# return the shortest path and distance from the question statement
# to each item
paths_dict = question_paths_in_graph(artcol, first_q)

# shallow question answering justified questions in justify.txt, using
# laws in lexml/ and getting the questions at OAB/raw/
result = sqa_justified_questions('doc/justify.txt', 'lexml/', 'OAB/raw/', rm_stopwords=True, separate=False)

# shallow question answering non-justified questions in an exam
paths = sqa_questions_in_exam('/home/bruno/git/oab-exams/OAB/raw/2016-20a.xml', artcol, max_questions=10)

# calculate paths and write them to json
questions_in_exams_to_json('exams_path', artcol, max_questions=10)


"""


#
## reading XML
def parse_xml(path, parser=etree.XMLParser(remove_blank_text=True)):
    return etree.parse(path)

def elements_in_tree(tree, element_tag):
    assert isinstance(tree, etree._ElementTree)
    for element in tree.getiterator(tag=element_tag):
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

    def str_repr(self):
        if self.valid and self.justification:
            return "OAB:{}|Q{}|ans:{}|just:{}".format(self.exam, self.number, self.valid, self.justification)
        elif self.valid:
            return "OAB:{}|Q{}|ans:{}|just:{}".format(self.exam, self.number, self.valid, ".")
        else:
            return "OAB:{}|Q{}|ans:{}".format(self.exam, self.number, "NULL")

    def __repr__(self):
        return self.str_repr()

def questions_in_tree(tree):
    for question in elements_in_tree(tree, 'question'):
        yield OABQuestion(question.get('number'),
                          get_exam_id(tree),
                           get_correct_item(question),
                               get_statement_text(question),
                               make_items_dict(get_items(question)))


#
## reading law XML

# lexML namespaces

def namespace_it(namespace, key, element):
    # namespaced element in {namespace}element syntax
    return "{{{}}}{}".format(namespace[key], element)

def lazy_articles_in_tree(tree):
    for artigo in elements_in_tree(tree, namespace_it(tree.getroot().nsmap, None, 'Artigo')):
        yield artigo.get('id'), ''.join(artigo.itertext())

def articles_in_tree(tree):
    return list(lazy_articles_in_tree(tree))

def get_urn(law_xml):
    assert isinstance(law_xml, etree._ElementTree)
    # fixme http://lxml.de/xpathxslt.html#namespaces-and-prefixes
    id_element = law_xml.find(namespace_it(law_xml.getroot().nsmap, None, 'Metadado') + '/' + namespace_it(law_xml.getroot().nsmap, None, 'Identificacao'))
    return id_element.get('URN')

def law_articles_in_file(law_path):
    law_xml = parse_xml(law_path)
    law_urn = get_urn(law_xml)
    return (law_urn, articles_in_tree(law_xml))

def all_law_articles_in_path(laws_path):
    # reads all .xml files in laws_path to a list of law_articles
    assert os.path.isdir(laws_path)
    laws = []
    for file in os.scandir(laws_path):
        if file.name.endswith(".xml"):
            law = law_articles_in_file(file.path)
            laws.append(law)
    return laws

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

def cosine_similarity(vec1, vec2):
    denominator = numpy.linalg.norm(vec1) * numpy.linalg.norm(vec2)
    if denominator == 0:
        return 0
    else:
        return numpy.dot(vec1, vec2) / denominator

class ArticleCollection(nltk.TextCollection):
    # source is [(law-urn [(art-id, raw-art-text)+])+]
    def __init__(self, source, rm_stopwords=False, text_preprocessing_fn=preprocess_text, similarity_fn=cosine_similarity):
        assert isinstance(source, list)
        self.rm_stopwords = rm_stopwords
        self._text_preprocessing_fn = text_preprocessing_fn
        self._similarity_fn = similarity_fn
        # map article id to its index
        self.ids, self.raw_texts = self.make_ids_and_raw_texts(source)
        self.laws = [law[0] for law in source]
        # remove law id
        # so that we have useful methods such as .idf(token)
        nltk.TextCollection.__init__(self, list(map(lambda x: text_preprocessing_fn(x, self.rm_stopwords), self.raw_texts)))
        # index tokens to create TF-IDF vector
        self.token_index_dict = {key:ix for ix, key in enumerate(self.vocab().keys())}
        self.vocab_size = len(self.vocab().keys())
        self.tfidf_vectors = [self.tfidf_vectorize(text) for text in self._texts]
        self.size = len(self._texts)
        # graph w/ only the articles as nodes, no edges
        self.base_graph = self.make_base_graph()

    def __repr__(self):
        return "ArticleCollection: {}".format(self.laws)

    def make_ids_and_raw_texts(self, source):
        ids = {}
        raw_texts = []
        ix = 0
        for law in source:
            law_id = law[0]
            for article in law[1]:
                art_id = article[0]
                art_id = law_id + art_id
                ids[art_id] = ix
                raw_texts.append(article[1])
                ix += 1
        return ids, raw_texts

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
        similarity = self._similarity_fn(vec1, vec2)
        if similarity == 0:
            return numpy.Infinity
        else:
            return 1 / similarity

    def make_base_graph(self):
        graph = networkx.DiGraph()
        graph.add_nodes_from(self.ids.keys())
        return graph


#
## add questions

def add_temporary_node(graph, artcol, text, label, to_nodes=True):
    """
    article_collection is where graph and tfidf-calculation happen,
    text is raw question statement (which is preprocessed here) and
    label is question number in str.
    to_nodes is the direction of the edges to be built. should be 
    from new node to the nodes already present, or from them to the
    node being added?
    """
    graph.add_node(label)
    label_tfidf = artcol.tfidf_vectorize(artcol._text_preprocessing_fn(text, artcol.rm_stopwords))
    # to add edges only to the articles, and not every node
    for node_id in artcol.ids.keys():
        node_ix = artcol.ids[node_id]
        if to_nodes:
            graph.add_edge(label, node_id, weight=artcol.inverse_similarity(label_tfidf, artcol.tfidf_vectors[node_ix]))
        else:
            graph.add_edge(node_id, label, weight=artcol.inverse_similarity(label_tfidf, artcol.tfidf_vectors[node_ix]))
    return graph

def question_paths_in_graph(article_collection, oab_question):
    """
    return distance and shortest path from statement to each item in
    oab_question.
    note that '1' (str) means question one.
    """
    assert isinstance(article_collection, ArticleCollection)
    assert isinstance(oab_question, OABQuestion)
    # so that base_graph is not changed improperly:
    graph = copy.deepcopy(article_collection.base_graph)
    # add question statement:
    graph = add_temporary_node(graph, article_collection, oab_question.statement, oab_question.number, to_nodes=True)
    paths = {}
    for question_letter, item_text in oab_question.items.items():
        graph = add_temporary_node(graph, article_collection, item_text, question_letter, to_nodes=False)
        paths[question_letter] = networkx.algorithms.shortest_paths.bidirectional_dijkstra(graph, oab_question.number, question_letter, weight='weight')
    return paths

#
## add justified questions

def read_laws_into_separate_artcol(laws_path, rm_stopwords):
    laws = {}
    for file in os.scandir(laws_path):
        if file.name.endswith(".xml"):
            urn, artigos = law_articles_in_file(file.path)
            artcol = ArticleCollection([(urn, artigos)], rm_stopwords)
            laws[urn] = artcol
    return laws

def read_laws_into_artcollection(laws_path, separate, rm_stopwords=False):
    # reads all .xml files in laws_path to a dictionary of urn:artcol
    assert os.path.isdir(laws_path)
    if separate:
        laws = read_laws_into_separate_artcol(laws_path, rm_stopwords)
    else:
        laws_list = all_law_articles_in_path(laws_path)
        laws = ArticleCollection(laws_list, rm_stopwords)
    return laws

def get_law_artcol(laws, urn, separate):
    if separate:
        return laws[urn]
    else:
        return laws

def find_question(oab_exam, question_nr):
    assert isinstance(oab_exam, etree._ElementTree)
    for question in questions_in_tree(oab_exam):
        if question.number == question_nr:
            return question

def sqa_justified_questions(justification_path, laws_path, exams_path, rm_stopwords=False, separate=True):
    # sqa = shallow question answering
    # justification file must be in the format described in docs.
    assert os.path.isfile(justification_path)
    assert os.path.isdir(exams_path)
    laws = read_laws_into_artcollection(laws_path, separate, rm_stopwords)
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
            artcol = get_law_artcol(laws, row[3], separate)
            # row[2]: justification article
            question.justification = (row[3], row[2])
            paths = question_paths_in_graph(artcol, question)
            question_paths[question] = paths
        return question_paths

def get_minimum_paths(question_paths):
    minimum_paths = {}
    for question, item_paths in question_paths.items():
        paths = []
        for item, item_path in item_paths.items():
            paths.append(item_path)
        minimum_path = reduce(lambda x,y: y if x[0]>y[0] else x if x[0] < y[0] else x + ("can't decide between {} and {}".format(x[1],y[1]),), paths)
        minimum_paths[question] = minimum_path
    return minimum_paths

def get_correct_item_paths(question_paths):
    correct_paths = {}
    for question, item_paths in question_paths.items():
        if not question.valid:
            continue
        correct_letter = question.valid
        correct_item_path = item_paths[correct_letter]
        correct_paths[question] = correct_item_path
    return correct_paths

def check_justification_correct_items(question_paths):
    # return True if justification for the correct article match with
    # the correct justification
    correct_items = {}
    for question, item_paths in question_paths.items():
        correct_letter = question.valid
        correct_item_path = item_paths[correct_letter]
        selected_article = correct_item_path[1][1]
        justification_urn = question.justification[0]
        justification_articles = question.justification[1].split(',')
        justification = list(map(lambda x: justification_urn + x, justification_articles))
        correct_items[question] = (selected_article in justification)
    return correct_items


#
## assign article to question

def sqa_questions_in_exam(exam_path, artcol, max_questions=-1):
    assert os.path.isfile(exam_path)
    exam = parse_xml(exam_path)
    question_paths = {}
    for ix, question in enumerate(questions_in_tree(exam)):
        if ix == max_questions:
            break
        paths = question_paths_in_graph(artcol, question)
        question_paths[question] = paths
    return question_paths

def make_paths_printable(question_paths):
    printable_paths = {}
    for question, item_paths in question_paths.items():
        question_str = question.str_repr()
        printable_paths[question_str] = item_paths
    return printable_paths

def to_json(dictionary, path):
    with open(path, 'w') as f:
        json.dump(dictionary, f, indent=4)

def questions_in_exams_to_json(exams_path, artcol, max_questions=-1):
    # make this work with all functions later
    assert os.path.isdir(exams_path)
    paths = {}
    for file in os.scandir(exams_path):
        if file.name.endswith(".xml"):
             exam_question_paths = sqa_questions_in_exam(file.path, artcol, max_questions=max_questions)
             paths[file.name] = make_paths_printable(exam_question_paths)
    result_path = os.path.join(os.path.dirname(file.path), 'results.json')
    to_json(paths, result_path)
