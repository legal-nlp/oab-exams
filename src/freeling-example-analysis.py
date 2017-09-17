from retrieval import *
import freeling
import sys
import re

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
# # create language analyzer
# la=freeling.lang_ident(DATA+"common/lang_ident/ident-few.dat");

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

# create tagger, sense anotator, and parsers

tg=freeling.hmm_tagger(DATA+LANG+"/tagger.dat",True,2);
sen=freeling.senses(DATA+LANG+"/senses.dat");
parser= freeling.chart_parser(DATA+LANG+"/chunker/grammar-chunk.dat");
# dep=freeling.dep_treeler(DATA+LANG+"/dep_treeler/dependences.dat", parser.get_start_symbol());
dep=freeling.dep_treeler(DATA+LANG+"/dep_treeler/dependences.dat");
ukb = freeling.ukb(DATA+LANG+"/ukb.dat");


def clean_article(article_string):
    # Very simple.
    # It should eliminate "Parágrafo único" and other structural parts
    # of the text. (or perhaps this should happen at parse time)
    return re.sub("  +","",
                  (re.sub("Art. [0-9]+\.","",
                          article_string.replace("\n",""))))

def show_senses(sentence):
    return list(map((lambda x: (x[0][0][0],x[1])),
                    filter((lambda x: len(x[0]) != 0),
                           map((lambda w: (w.get_senses(),
                                           w.get_form())), sentence.get_words()))))

# First initial experiment
lei = law_articles_in_file('./my-laws-xml/codigo-de-etica.xml')
sent = lei[1][23][1]
sent = clean_article(sent)


analysis = tk.tokenize(sent)
analysis = sp.split(sid,analysis,False)

analysis = mf.analyze(analysis)
analysis = tg.analyze(analysis)
analysis = sen.analyze(analysis)
analysis = ukb.analyze(analysis)
analysis = parser.analyze(analysis)
analysis = dep.analyze(analysis)


# def run():
#     x = clean_article(lei[1][22][1])
#     x = tk.tokenize(x)
#     x = sp.split(sid,x,False)
#     x = mf.analyze(x)
#     x = sen.analyze(x)
#     return x
