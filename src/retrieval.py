from lxml import etree

def parse_xml(path):
    return etree.parse(path)

def questions_in_tree(tree_root):
    for question in tree_root.getiterator(tag="question"):
        yield question

def get_statement_text(question):
    return question.find('statement').text

def get_items_text(question):
    return list(map(lambda i: getattr(i, 'text'), question.find('items').getchildren()))

def parsed_questions_in_tree(tree_root):
    for question in questions_in_tree(tree_root):
        if question.get('valid') == 'true':
            yield [get_statement(question), get_items(question)]
            
