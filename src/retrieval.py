from lxml import etree


#
## reading XML
def parse_xml(path, parser=etree.XMLParser(remove_blank_text=True)):
    return etree.parse(path)

def elements_in_tree(tree_root, element_tag):
    for element in tree_root.getiterator(tag=element_tag):
        yield element


#
## reading OAB exams
def get_statement_text(question):
    return question.find('statement').text

def get_items(question):
    return question.find('items').getchildren()

def make_items_dict(items):
    return dict((i.get('letter'),
                (i.get('correct'), getattr(i, 'text'))) for i in items)

class OABQuestion():
    def __init__(self, number, valid, statement, items):
        self.number = number
        self.valid = valid
        self.statement = statement
        self.items = items

def questions_in_tree(tree_root):
    for question in elements_in_tree(tree_root, 'question'):
        yield OABQuestion(question.get('number'),
                           question.get('valid'),
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
    return "{{{}}}{}".format(namespaces[key], element)

def lazy_articles_in_tree(tree_root, namespace=nsm):
    for artigo in elements_in_tree(tree_root, namespace_it(namespace, None, 'Artigo')):
        yield artigo.get('id'), ''.join(artigo.itertext())

def articles_in_tree(tree_root, namespace=nsm):
    return list(lazy_articles_in_tree(tree_root, namespace))

"""
# examples

oab = parse_xml('/home/bruno/git/oab-exams/src/2010-01.xml')
questions = questions_in_tree(oab)
next(questions)

lei = parse_xml('/home/bruno/git/oab-exams/lexml/lei-8906.xml')
artigos =  articles_in_tree(lei)
next(artigos)

"""

#
## text processing
import nltk
nltk.download('punkt')
  
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
    assert bool(rm_stopwords)
    return [token.lower().strip() for token in nltk.tokenize.word_tokenize(text) if is_useful(token, rm_stopwords)]


#
## tf-idf and base graph making
import collections
import numpy
import networkx

class ArtigosCollection(nltk.TextCollection):
    def __init__(self, source, rm_stopwords):
        self.rm_stopwords = rm_stopwords
        self.ids = {ix:artigo[0] for ix, artigo in enumerate(source)}
        nltk.TextCollection.__init__(self, list(map(lambda x: preprocess_text(x[1], rm_stopwords), source)))
        self.token_index_dict = {key:ix for ix, key in enumerate(self.vocab().keys())}
        self.vocab_size = len(self.vocab().keys())
        self.tfidf_vectors = [self.tfidf_vectorize(text) for text in self._texts]
        self.size = len(self._texts)
        self.base_graph = self.make_base_graph()

    def tf_tokens(self, tokens):
        count = collections.Counter(tokens)
        length = len(tokens)
        return list(map(lambda x: count[x]/length, tokens))

    def tfidf_vectorize(self, text):
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

    def add_edges_from(self, graph, node_ix):
        node_tfidf = self.tfidf_vectors[node_ix]
        for adj_ix in range(node_ix + 1, self.size):
            graph.add_edge(node_ix, adj_ix, weight = self.inverse_similarity(node_tfidf, self.tfidf_vectors[adj_ix]))
        return graph

    def make_base_graph(self):
        graph = networkx.Graph()
        graph.add_nodes_from(range(self.size))
        for node in range(self.size):
            graph = self.add_edges_from(graph, node)
        return graph


def cosine_similarity(vec1, vec2):
    denominator = numpy.linalg.norm(vec1) * numpy.linalg.norm(vec2)
    if denominator == 0:
        return 0
    else:
        return numpy.dot(vec1, vec2) / denominator

"""
examples:

artcol = ArtigosCollection(artigos, rm_stopwords=True)

"""

#
## add questions
"""
networkx.algorithms.shortest_paths.dijkstra_path()
networkx.algorithms.shortest_paths.generic.has_path()
...bidirectional_dijkstra
https://networkx.github.io/documentation/stable/reference/generated/networkx.algorithms.shortest_paths.generic.all_shortest_paths.html#networkx.algorithms.shortest_paths.generic.all_shortest_paths
"""

def add_temporary_node(artigos_collection, text, label, graph=None):
    """
    artcol is where graph and tfidf-calculation happen, text is raw
    question statement (which is preprocessed here) and label is
    question number in str (if it were int it would shadow an existing
    node).
    """
    if graph is None:
        graph = artigos_collection.base_graph
    graph.add_node(label)
    label_tfidf = artigos_collection.tfidf_vectorize(preprocess_text(text, artigos_collection.rm_stopwords))
    for node in artigos_collection.ids.keys():
        graph.add_edge(label, node, weight=artigos_collection.inverse_similarity(label_tfidf, artigos_collection.tfidf_vectors[node]))
    return graph

# gotta check if deepcopy is needed

def add_question_to_graph(artigos_collection, oab_question):
    assert isinstance(artigos_collection, ArtigosCollection)
    assert isinstance(oab_question, OABQuestion)
    graph = add_temporary_node(artigos_collection, oab_question.statement, oab_question.number)
    paths = {}
    for question_item, item in oab_question.items.items():
        graph2 = add_temporary_node(artigos_collection, item[1], question_item, graph=graph)
        paths[question_item] = list(networkx.algorithms.shortest_paths.generic.all_shortest_paths(graph2, oab_question.number, question_item, weight='weight'))
    return paths
