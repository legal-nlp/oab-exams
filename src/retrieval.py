from lxml import etree

#
## reading XML
def parse_xml(path, parser=etree.XMLParser(remove_blank_text=True)):
    return etree.parse(path)

def elements_in_tree(tree_root, element):
    for question in tree_root.getiterator(tag=element):
        yield question


#
## reading OAB exams
def get_statement_text(question):
    return question.find('statement').text

def get_items(question):
    return question.find('items').getchildren()

def make_items_dict(items):
    return dict((i.get('letter'),
                (i.get('correct'), getattr(i, 'text'))) for i in items)

def questions_in_tree(tree_root):
    for question in elements_in_tree(tree_root, 'question'):
        if question.get('valid') == 'true':
            question_dict = {question.get('number'): get_statement_text(question)}
            question_dict.update(make_items_dict(get_items(question)))
            yield  question_dict

#
## reading law XML

# lexML namespaces
nsm = {"xlink": "http://www.w3.org/1999/xlink",
           'xsi': 'http://www.w3.org/2001/XMLSchema-instance',
           "xml": "http://www.w3.org/XML/1998/namespace",
           None: "http://www.lexml.gov.br/1.0"}

def namespace_it(namespaces, key, element):
    return "{{{}}}{}".format(namespaces[key], element)

def articles_in_tree(tree_root, namespace=nsm):
    for artigo in elements_in_tree(tree_root, namespace_it(namespace, None, 'Artigo')):
        yield artigo.get('id'), ''.join(artigo.itertext())

"""
# examples

oab = parse_xml('/home/bruno/git/oab-exams/src/2010-01.xml')
questions = parsed_questions_in_tree(oab)
next(questions)

lei = parse_xml('/home/bruno/git/oab-exams/lexml/lei-8906.xml')
artigos =  articles_in_tree(lei)
next(artigos)

"""

#
## tf-idf
import nltk
import numpy
import collections
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

class ArtigosCollection(nltk.TextCollection):
    def __init__(self, source, rm_stopwords=False):
        self.ids = {artigo[0]:ix for ix, artigo in enumerate(source)}
        nltk.TextCollection.__init__(self, list(map(lambda x: preprocess_text(x[1], rm_stopwords), source)))
        self.index_dict = {key:ix for ix, key in enumerate(self.vocab().keys())}
        self.vocab_size = len(self.vocab().keys())
        self.tfidf_vectors = [self.tfidf_vectorize(text) for text in self._texts]

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
            tfidf_vector[self.index_dict[token]] = tf_vector[ix] * idf
        return tfidf_vector

def cosine_similarity(vec1, vec2):
    denominator = numpy.linalg.norm(vec1) * numpy.linalg.norm(vec2)
    if denominator == 0:
        return 0
    else:
        return numpy.dot(vec1, vec2) / denominator

"""
examples:

arts = preprocess_artigos(artigos, rm_stopwords=True)
artcol = ArtigosCollection(arts)

"""
