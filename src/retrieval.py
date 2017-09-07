from lxml import etree

##
## reading oab XML
def parse_xml(path):
    return etree.parse(path)

def questions_in_tree(tree_root):
    for question in tree_root.getiterator(tag="question"):
        yield question

def get_attribute(element, attribute):
    return element.get(attribute)

def get_statement_text(question):
    return question.find('statement').text

def get_items(question):
    return question.find('items').getchildren()

def make_items_dict(items):
    return dict((i.get('letter'),
                (i.get('correct'), getattr(i, 'text'))) for i in items)

def parsed_questions_in_tree(tree_root):
    for question in questions_in_tree(tree_root):
        if question.get('valid') == 'true':
            question_dict = {question.get('number'): get_statement_text(question)}
            question_dict.update(make_items_dict(get_items(question)))
            yield  question_dict
            
