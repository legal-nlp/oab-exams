from retrieval import *
import freeling
import sys
import re
import collections
import math

def prepare_freeling():    
    # Freeling:
    # https://github.com/TALP-UPC/FreeLing
    # (you may download binary at releases there)
    # (GPP: I'm using 4.0)
    
    # Make sure that the directory contanining libfreeling.so (FREELINGDIR/lib) is
    # in your LD_LIBRARY_PATH.
    
    # Make sure that freeling.py and _freeling.so are in the same directory as this one.
    # example of freeling's python API is at: https://github.com/TALP-UPC/FreeLing/tree/master/APIs/python
    
    # Change directories for your location
    FREELINGDIR = "/usr/local"; 
    DATA = FREELINGDIR+"/share/freeling/";
    
    LANG="pt";
    
    freeling.util_init_locale("default");
    
    # create options set for maco analyzer. Default values are Ok, except for data files.
    op= freeling.maco_options("pt");
    op.set_data_files( "", 
                       DATA + "common/punct.dat",
                       DATA + LANG + "/dicc.src",
                       DATA + LANG + "/afixos.dat",
                       "",
                       DATA + LANG + "/locucions.dat", 
                       DATA + LANG + "/np.dat",
                       "", # there's not "quantitites.dat" for pt 
                       DATA + LANG + "/probabilitats.dat");
    
    # create analyzers
    tk=freeling.tokenizer(DATA+LANG+"/tokenizer.dat");
    sp=freeling.splitter(DATA+LANG+"/splitter.dat");
    sid=sp.open_session();
    mf=freeling.maco(op);
    
    # activate mmorpho odules to be used in next call
    mf.set_active_options(False, True, True, True,  # select which among created 
                          True, True, False, True,  # submodules are to be used. 
                          True, True, True, True ); # default: all created submodules are used
    
    # create tagger, sense anotator, and ukb
    
    tg=freeling.hmm_tagger(DATA+LANG+"/tagger.dat",True,2);
    sen=freeling.senses(DATA+LANG+"/senses.dat");
    parser= freeling.chart_parser(DATA+LANG+"/chunker/grammar-chunk.dat");
    ukb = freeling.ukb(DATA+LANG+"/ukb.dat");
    outputter = freeling.output_conll('./output_conll.dat')
    return tk, sp, sid, mf, tg, sen, parser, ukb, outputter

# tokenizer, sentence splitter, morphological, tagger, sense labeller, word sense disambiguator (ukb algorithm), output
tk, sp, sid, mf, tg, sen, parser, ukb, outputter = prepare_freeling()

def clean_article(article_string):
    # Very simple.
    # It should eliminate "Parágrafo único" and other structural parts
    # of the text. (or perhaps this should happen at parse time)
    return re.sub("  +","",
                  (re.sub("Art. [0-9]+(\.|º)","",
                          article_string.replace("\n",""))))

def sqa_justified_synset_approach(justification_path, laws_path, exams_path):
    # sqa = shallow question answering
    # justification file must be in the format described in docs.
    # see ./retrieval.py
    assert os.path.isfile(justification_path)
    assert os.path.isdir(exams_path)
    laws = fl_read_laws_into_artcollection(laws_path)
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
            paths = question_paths_in_sense_graph(artcol, question)
            question_paths[question] = paths
        return question_paths

def analyze_text(input_text):
    assert(isinstance(input_text,str))
    text = clean_article(input_text)
    text = tk.tokenize(text)
    text = sp.split(sid,text,False)
    text = mf.analyze(text)
    text = tg.analyze(text)
    text = sen.analyze(text)
    text = ukb.analyze(text)
    return text

# THIS IS THE CORRECT IMPLEMENTATION; commented just for debugging
def get_senses_from_text(input_text):
    assert(isinstance(input_text,str))
    text = analyze_text(input_text)
    senses = {}
    for sentence in text:
        for word in sentence.get_words():
            total = 0
            for sense_pair in word.get_senses():
                # sense_pair is (sense, value)
                total += sense_pair[1]
            for sense_pair in word.get_senses():
                if sense_pair[0] in senses:
                    senses[sense_pair[0]] += sense_pair[1]/total
                else:
                    senses[sense_pair[0]] = sense_pair[1]/total
    return senses

def analyze_list_of_text(input_list_text):
    # input list of text
    # output list of list of sentences
    assert(isinstance(input_list_text,list) or isinstance(input_list_text,tuple))
    for i in input_list_text:
        assert(isinstance(i,str))
    list_analyzed = [sen.analyze(tg.analyze(mf.analyze(sp.split(sid,tk.tokenize(clean_article(text)), False)))) for text in input_list_text]
        # list_analyzed does NOT contain WSD senses yet
    all_sentences = []
    entry_has_sentences = {i:[] for i in range(len(list_analyzed))}
    counter = 0
    for entry_index in range(len(list_analyzed)):
        for sentence in list_analyzed[entry_index]:
            all_sentences.insert(len(all_sentences),sentence)
            entry_has_sentences[entry_index].insert(len(entry_has_sentences[entry_index]), counter)
            # sentence_in_entry[counter] = entry_index
            counter += 1
            # entry_has_sentences[i] means that the entry indexed by i
            # contains exactly the sentences in all_sentences indexed
            # by values in entry_has_sentences[i]
    all_sentences = ukb.analyze(all_sentences)
    output_analysis = [[] for i in range(len(list_analyzed))]
    for entry_index in range(len(list_analyzed)):
        new_sentences = []
        for sentence in [all_sentences[i] for i in entry_has_sentences[entry_index]]:
            words = []
            for word in sentence.get_words():
                weight_sum = 0
                for sense in word.get_senses():
                    weight_sum += sense[1]
                word.set_senses(list(map(
                    (lambda x: [x[0],x[1]/weight_sum]),
                    word.get_senses())))
                words.insert(len(words),word)
            new_sentences.insert(len(new_sentences),freeling.sentence(words))
                # It was necessary to create new sentences because freeling
                # recreates the original words using the .get_words() method,
                # making the .sense_senses before irrelevant
        output_analysis[entry_index] = new_sentences
    return output_analysis
    
def get_senses_from_list_of_text(input_list_text):
    # Receives a list of strings, returns a list of dictionaries of
    # senses (value:key is sense:weight)
    analyzed_texts = analyze_list_of_text(input_list_text)
    output_senses = [{} for i in analyzed_texts]
    for entry_index in range(len(analyzed_texts)):
        senses = {}
        for sentence in analyzed_texts[entry_index]:
            for word in sentence.get_words():
                for sense_pair in word.get_senses():
                    if sense_pair[0] in senses:
                        senses[sense_pair[0]] += sense_pair[1]
                    else:
                        senses[sense_pair[0]] = sense_pair[1]
        output_senses[entry_index] = senses
    return tuple(output_senses)

    # for entry_index in range(len(list_analyzed)):
    #     senses = {}
    #     for sentence in [all_sentences[i] for i in entry_has_sentences[entry_index]]:
    #         for word in sentence.get_words():
    #             total = 0
    #             for sense_pair in word.get_senses():
    #                 # sense_pair is (sense, value)
    #                 total += sense_pair[1]
    #             for sense_pair in word.get_senses():
    #                 if sense_pair[0] in senses:
    #                     senses[sense_pair[0]] += sense_pair[1]/total
    #                 else:
    #                     senses[sense_pair[0]] = sense_pair[1]/total
    #     output_analysis[entry_index] = senses
    # return tuple(output_analysis)

# this uses FORMS not senses
#   (was very important for sanity check)
# def get_senses_from_text(input_text):
#     assert(isinstance(input_text,str))
#     text = analyze_text(input_text)
#     senses = {}
#     for sentence in text:
#         for word in sentence.get_words():
#             # total = 0
#             # for sense_pair in word.get_senses():
#             #     # sense_pair is (sense, value)
#             #     total += sense_pair[1]
#             if True:
#                 form = word.get_form()
#                 if form in senses:
#                     senses[form] += 1
#                 else:
#                     senses[form] = 1
#     return senses


def apply_to_law_text(func, law):
    law_urn, articles = law
    art_nrs, art_txts = zip(*articles)
    applied_art_txts = func(art_txts)
    applied_articles = list(zip(art_nrs, applied_art_txts))
    return law_urn, applied_articles

def get_article_senses(article):
    artnr, arttext = article
    return artnr, get_senses_from_text(arttext)

# def get_law_senses(law_articles):
#     law_urn, articles = law_articles
#     return law_urn, list(map(get_article_senses, articles))

def get_law_senses(law_articles):
    # This does joint WSD
    return apply_to_law_text(get_senses_from_list_of_text,law_articles)


def write_conll_law_text(law, output_file_name):
    # This uses WSD for all sentences together
    law_urn, articles = law
    art_nrs, art_txts = zip(*articles)
    with open(output_file_name, 'w') as f:
        write_conll_justified_sentences
        f.write(write_freeling_sentences_analysis_conll(art_txts))
    return None

def write_freeling_sentences_analysis_conll(input_list_text):
    # Please don't use this out of context.
    # this is pretty terrible
    assert(isinstance(input_list_text,list) or isinstance(input_list_text,tuple))
    for i in input_list_text:
        assert(isinstance(i,str))
    list_analyzed = [sen.analyze(tg.analyze(mf.analyze(sp.split(sid,tk.tokenize(clean_article(text)), False)))) for text in input_list_text]
    # list_analyzed does NOT contain WSD senses yet
    all_sentences = []
    entry_has_sentences = {i:[] for i in range(len(list_analyzed))}
    counter = 0
    for entry_index in range(len(list_analyzed)):
        for sentence in list_analyzed[entry_index]:
            all_sentences.insert(len(all_sentences),sentence)
            entry_has_sentences[entry_index].insert(len(entry_has_sentences[entry_index]), counter)
            # sentence_in_entry[counter] = entry_index
            counter += 1
            # entry_has_sentences[i] means that the entry indexed by i
            # contains exactly the sentences in all_sentences indexed
            # by values in entry_has_sentences[i]
    all_sentences = ukb.analyze(all_sentences)
    #
    new_sentences = []
    for sentence in all_sentences:
        words = []
        for word in sentence.get_words():
            weight_sum = 0
            for sense in word.get_senses():
                weight_sum += sense[1]
            word.set_senses(list(map(
                (lambda x: [x[0],x[1]/weight_sum]),
                word.get_senses())))
            words.insert(len(words),word)
        new_sentences.insert(len(new_sentences),freeling.sentence(words))
        # It was necessary to create new sentences because freeling
        # recreates the original words using the .get_words() method,
        # making the .sense_senses before irrelevant
    text = tuple(new_sentences)
    return outputter.PrintResults(text)

def write_freeling_analysis_conll(input_text):
    # Please don't use this out of context.
    # this is pretty terrible
    assert(isinstance(input_text,str))
    text = clean_article(input_text)
    text = tk.tokenize(text)
    text = sp.split(sid,text,False)
    text = mf.analyze(text)
    text = tg.analyze(text)
    text = sen.analyze(text)
    text = ukb.analyze(text)
    new_sentences = []
    for sentence in text:
        words = []
        for word in sentence.get_words():
            # print(word.get_form())
            weight_sum = 0
            for sense in word.get_senses():
                weight_sum += sense[1]
            word.set_senses(list(map(
                # (lambda x: [str(x[0]) + ":" + str((x[1]/weight_sum)),x[1]/weight_sum]),
                (lambda x: [x[0],x[1]/weight_sum]),
                word.get_senses())))
            words.insert(len(words),word)
        new_sentences.insert(len(new_sentences),freeling.sentence(words))
        # It was necessary to create new sentences because freeling
        # recreates the original words using the .get_words() method,
        # making the .sense_senses before irrelevant
    text = tuple(new_sentences)
    # for sentence in text:
    #     for word in sentence.get_words():
    #         print(word.get_form())
    #         print(word.get_senses())
    # output = open(outputdir + outputname, 'w')
    # output.write(outputter.PrintResults(text))
    # output.close()
    return outputter.PrintResults(text)

def add_temporary_sense_node(graph, artcol, text_senses, label, to_nodes=True):
    """
    article_collection is where graph and tfidf-calculation happen and
    label is question number in str.
    text_senses is dict {sense: weight} (already processed question statement)
    to_nodes is the direction of the edges to be built. should be 
    from new node to the nodes already present, or from them to the
    node being added?

    "sense" means it's relative to the senses found by freeling in the
    analysis (and not to the tf-idf of the words themselves).
    """
    graph.add_node(label)
    # text_senses should be dict {sense:weight}
    # text_senses = get_senses_from_text(text)
    label_tfidf = artcol.tfidf_vectorize(text_senses)
    # to add edges only to the articles, and not every node
    for node_id in artcol.ids.keys():
        node_ix = artcol.ids[node_id]
        # print("ADD TEMPORARY NODE")
        # print("node_id = {}, node_ix = {}, label = {}".format(node_id,node_ix,label))
        # print("===")
        if to_nodes:
            graph.add_edge(label, node_id, weight=artcol.inverse_similarity(label_tfidf, artcol.tfidf_vectors[node_ix]))
        else:
            graph.add_edge(node_id, label, weight=artcol.inverse_similarity(label_tfidf, artcol.tfidf_vectors[node_ix]))
    return graph

def question_paths_in_sense_graph(article_collection, oab_question):
    """
    return distance and shortest path from statement to each item in
    oab_question.
    note that '1' (str) means question one.

    "sense" means it's relative to the senses found by freeling in the
    analysis (and not to the tf-idf of the words themselves).
    """
    assert isinstance(article_collection, SenseArticleCollection)
    assert isinstance(oab_question, OABQuestion)
    # so that base_graph is not changed improperly:
    graph = copy.deepcopy(article_collection.base_graph)
    # process questions
    # index 0 is question, 1, 2, 3 and 4 are resp. A, B, C and D
    oab_question_senses = get_senses_from_list_of_text([oab_question.statement, oab_question.items['A'], oab_question.items['B'], oab_question.items['C'], oab_question.items['D']])
    oab_question_senses = dict(zip(['quest','A','B','C','D'], oab_question_senses))
    # add question statement:
    graph = add_temporary_sense_node(graph, article_collection, oab_question_senses['quest'], oab_question.number, to_nodes=True)
    paths = {}
    for question_letter, item_text in oab_question.items.items():
        graph = add_temporary_sense_node(graph, article_collection, oab_question_senses[question_letter], question_letter, to_nodes=False)
        paths[question_letter] = networkx.algorithms.shortest_paths.bidirectional_dijkstra(graph, oab_question.number, question_letter, weight='weight')
    return paths

class SenseArticleCollection():
    def __init__(self, laws, similarity_fn=cosine_similarity):
        assert isinstance(laws, list)
        self._similarity_fn = similarity_fn
        # map article id to its index
        self.ids, self.articles = self.separate_ids_and_articles(laws)
        self.laws = [law[0] for law in laws]
        self.size = len(self.articles)
        self.dfs = self.make_dfs()
        self.sense_indices = {key:ix for ix, key in enumerate(self.dfs.keys())}
        self.vocab_size = len(self.sense_indices.keys())
        self.tfidf_vectors = [self.tfidf_vectorize(article_senses) for article_senses in self.articles]
        self.base_graph = self.make_base_graph()

    def separate_ids_and_articles(self, laws):
        ids = {}
        articles = []
        ix = 0
        for law in laws:
            law_id = law[0]
            for article in law[1]:
                art_id = article[0]
                art_id = law_id + art_id
                ids[art_id] = ix
                articles.append(article[1])
                ix += 1
        return ids, articles

    def make_dfs(self):
        dfs = collections.defaultdict(lambda: 0, {})
        for article in self.articles:
            art_vocab = set()
            for sense in article.keys():
                art_vocab.add(sense)
            for sense in art_vocab:
                if article[sense] >= 1:
                    dfs[sense] += 1
                else:
                    dfs[sense] += article[sense]
        return dfs
    
    def tf_tokens(self, tokens):
        # this function if adapted to input like this
        # {'sense-a':0.5, 'sense-b':0.8, 'sense-c':2}
        length = sum(map(lambda x: tokens[x], tokens.keys()))
        return list(map(lambda x: tokens[x]/length, tokens.keys())) # tf adjusted by document length

    def tfidf_vectorize(self, article):
        tfidf_vector = numpy.zeros(self.vocab_size)
        tf_vector = self.tf_tokens(article)
        for ix, (sense, weight) in enumerate(article.items()):
            df = self.dfs[sense]
            if df == 0:
                continue
            tfidf_vector[self.sense_indices[sense]] = tf_vector[ix] * math.log(self.size/df)
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

def fl_read_laws_into_artcollection(laws_path):
    laws = {}
    assert os.path.isdir(laws_path)
    laws_list = all_law_articles_in_path(laws_path)
    law_senses = list(map(get_law_senses, laws_list))
    laws = SenseArticleCollection(law_senses)
    return laws
    
# def read_laws_into_separate_senseartcol(laws_path, rm_stopwords):
#     laws = {}
#     for file in os.scandir(laws_path):
#         if file.name.endswith(".xml"):
#             urn, artigos = law_articles_in_file(file.path)
#             artcol = SenseArticleCollection([(urn, artigos)], rm_stopwords)
#             laws[urn] = artcol
#     return laws

# def read_laws_into_senseartcollection(laws_path, separate, rm_stopwords=False):
#     # reads all .xml files in laws_path to a dictionary of urn:artcol
#     assert os.path.isdir(laws_path)
#     if separate:
#         laws = read_laws_into_separate_senseartcol(laws_path, rm_stopwords)
#     else:
#         laws_list = all_law_articles_in_path(laws_path)
#         laws = SenseArticleCollection(laws_list, rm_stopwords)
#     return laws

def write_conll_justified_sentences(justification_path, laws_path, exams_path, output_name, output_path="./"):
    # justification file must be in the format described in docs.
    # see ./retrieval.py
    # currently not printin laws_path
    assert os.path.isfile(justification_path)
    assert os.path.isdir(exams_path)
    assert os.path.isdir(output_path)
    # laws = fl_read_laws_into_artcollection(laws_path)
    sent_file = open(output_path + output_name, 'w')
    # laws = dict(all_law_articles_in_path(laws_path))
    with open(justification_path, 'r') as tsv:
        tsv = csv.reader(tsv, delimiter='\t')
        for row in tsv:
            # row[0]: OAB exam filename
            sent_file.write("# STARTING NEW QUESTION:\n# {}\n".format(row))
            exam_path = os.path.join(exams_path, row[0] + '.xml')
            oab_exam = parse_xml(exam_path)
            # row[1]: question number
            # row[2]: justification article
            # row[3]: justification law URN
            question = find_question(oab_exam, row[1])
            question.justification = (row[3], row[2])
            analyzed_question = analyze_list_of_text([question.statement, question.items['A'], question.items['B'], question.items['C'], question.items['D']])
            analyzed_question = dict(zip(['quest','A','B','C','D'], analyzed_question))
            # articles_text = get_article_from_law(laws,row[3],row[2])
            # Write in the following order:
            # question, answer, justification
            sent_file.write("# Question statement\n")
            sent_file.write(outputter.PrintResults(analyzed_question['quest']))
            sent_file.write("# Question items\n")
            for item in ['A','B','C','D']:
              sent_file.write("# Item {}\n".format(item))
              if item == question.valid:
                  sent_file.write("# Question answer\n")            
              sent_file.write(outputter.PrintResults(analyzed_question[item]))
            sent_file.write("Justification is {}".format(question.justification))
            # sent_file.write("# Justifications (articles)\n")
            # for article_text in articles_text:
            #     sent_file.write(write_freeling_analysis_conll(article_text))
        return None

def get_article_from_law(laws, law, articles):
    # LAW as dict of what is returned from all_law_articles_in_path
    d = dict(laws[law])
    output = []
    for art in articles.split(','):
        output.insert(0, d[art])
    return output

def evaluate_correct_answer(answer):
    """Verifies which questions are correctly answered (that is, have
    minimum weight only in the correct alternative), wrongly answered
    or are undecided (minimum weight in the correct alternative and in
    at least some other)"""
    # Input: dictionary indexed by questions ("question_paths")
    correct = []
    wrong = []
    undecided = []
    for question in answer.keys():
        correct_opt = question.valid
        q_result = answer[question]
        min_weight = np.min([q_result[opt][0] for opt in q_result.keys()])
        min_alternatives = []
        for opt in q_result.keys():
            if q_result[opt][0] == min_weight:
                min_alternatives.insert(0,opt)
        assert len(min_alternatives) > 0 , "question {} has no minimum options, minimum is {}".format(question, min_weight)

        if correct_opt in min_alternatives:
            if len(min_alternatives) > 1:
                undecided.insert(0,question)
            else:
                correct.insert(0,question)
        else:
            wrong.insert(0,question)
    return correct, wrong, undecided
            
# def evaluate_justification(answer):
#     # Input: dictionary indexed by questions ("question_paths")
#     correct = []
#     wrong = []
#     undecided = []
#     for question in answer.keys():
#         correct_opt = question.valid
#         q_result = answer[question]
#         min_weight = np.min([q_result[opt][0] for opt in q_result.keys()])
#         min_alternatives = []
#         for opt in q_result.keys():
#             if q_result[opt][0] == min_weight:
#                 min_alternatives.insert(0,opt)
#         assert len(min_alternatives) > 0 , "question {} has no minimum options, minimum is {}".format(question, min_weight)

#         if correct_opt in min_alternatives:
#             if len(min_alternatives) > 1:
#                 undecided.insert(0,question)
#             else:
#                 correct.insert(0,question)
#         else:
#             wrong.insert(0,question)
#     return correct, wrong, undecided
    
