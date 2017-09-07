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

oab = parse_xml('~/git/oab-exams/src/2010-01.xml')
questions = parsed_questions_in_tree(oab)
next(questions)

lei = parse_xml('~/git/oab-exams/lexml/lei-8906.xml')
artigos =  articles_in_tree(a)
next(artigos)

"""
