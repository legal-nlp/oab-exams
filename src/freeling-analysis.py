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
    return tk, sp, sid, mf, tg, sen, parser, ukb

# tokenizer, sentence splitter, morphological, tagger, sense labeller, word sense disambiguator (ukb algorithm)
tk, sp, sid, mf, tg, sen, parser, ukb = prepare_freeling()

def clean_article(article_string):
    # Very simple.
    # It should eliminate "Parágrafo único" and other structural parts
    # of the text. (or perhaps this should happen at parse time)
    return re.sub("  +","",
                  (re.sub("Art. [0-9]+\.","",
                          article_string.replace("\n",""))))


def sqa_justified_synset_approach(justification_path, laws_path, exams_path, rm_stopwords=False, separate=True):
    # sqa = shallow question answering
    # justification file must be in the format described in docs.
    # see ./retrieval.py
    assert os.path.isfile(justification_path)
    assert os.path.isdirir(exams_path)
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
            paths = question_paths_in_sense_graph(artcol, question)
            question_paths[question] = paths
        return question_paths


    # for question in ...:
    #     merge_question_answer_text(question)
    #     clean_question_answer_text(question)

def get_senses_from_text(input_text):
    assert(isinstance(input_text,str))
    text = clean_article(input_text)
    text = tk.tokenize(text)
    text = mf.analyze(text)
    text = tg.analyze(text)
    text = sen.analyze(text)
    text = ukb.analyze(text)
    senses = {}
    for sentence in text:
        for word in sentence.get_words():
            total = 0
            for sense_pair in word.get_senses():
                # sense_pair is (sense, value)
                total += sense_pair[1]
            for sense_pair in word.get_senses():
                if sense in senses:
                    senses[sense_pair[0]] += sense_pair[1]/total
                else:
                    senses[sense_pair[0]] = sense_pair[1]/total
    return senses
    
    
def add_temporary_sense_node(graph, artcol, text, label, to_nodes=True):
    """
    article_collection is where graph and tfidf-calculation happen,
    text is raw question statement (which is preprocessed here) and
    label is question number in str.
    to_nodes is the direction of the edges to be built. should be 
    from new node to the nodes already present, or from them to the
    node being added?

    "sense" means it's relative to the senses found by freeling in the
    analysis (and not to the tf-idf of the words themselves).
    """
    graph.add_node(label)
    label_tfidf = artcol.tfidf_vectorize(artcol._text_preprocessing_fn(text, artcol.rm_stopwords))
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
    assert isinstance(article_collection, ArticleCollection)
    assert isinstance(oab_question, OABQuestion)
    # so that base_graph is not changed improperly:
    graph = copy.deepcopy(article_collection.base_graph)
    # add question statement:
    graph = add_temporary_sense_node(graph, article_collection, oab_question.statement, oab_question.number, to_nodes=True)
    paths = {}
    for question_letter, item_text in oab_question.items.items():
        graph = add_temporary_sense_node(graph, article_collection, item_text, question_letter, to_nodes=False)
        paths[question_letter] = networkx.algorithms.shortest_paths.bidirectional_dijkstra(graph, oab_question.number, question_letter, weight='weight')
    return paths

class SenseArticleCollection():
    def __init__(self, laws, similarity_fn=cosine_similarity):
        assert isinstance(laws, list)
        self._similarity_fn = similarity_fn
        # map article id to its index
        self.ids, self.articles = self.separate_ids_and_articles(laws)
        self.laws = [law[0] for law in laws]
        self.size = len(self.laws)
        self.dfs = self.make_dfs()
        self.sense_indices = {key:ix for ix, key in enumerate(self.dfs.keys())}
        self.vocab_size = len(self.sense_indices.keys())
        self.tfidf_vectors = [self.tfidf_vectorize(article_senses) for article_senses in self.articles]

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
                dfs[sense] += 1
        return dfs
    
    def tf_tokens(self, tokens):
        count = collections.Counter(tokens)
        length = len(tokens)
        return list(map(lambda x: count[x]/length, tokens))

    def tfidf_vectorize(self, article):
        tfidf_vector = numpy.zeros(self.vocab_size)
        tf_vector = self.tf_tokens(list(article.keys()))
        for ix, (sense, weight) in enumerate(article.items()):
            df = self.dfs[sense]
            if df == 0:
                continue
            tfidf_vector[self.sense_indices[sense]] = weight * tf_vector[ix] * math.log(self.size/df)
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
