import re

# abrir o arquivo com a justificativa das questões em txt
file = open("justify-ethics.txt","r") 

# a melhor forma de resolver isso: uma lista de dicionários, cada dicionário
# tem a análise de uma questão

questões_dic = []

for i in file:

	# criar o dicionário com as infos daquela questão
	dic_q = {}
	
	# ir poríndice é de boa porque não varia o tamanho da info que eu quero
	exame_val = i.strip('\t\n')[0:6]
	dic_q["exam"] = exame_val

	# ir poríndice é de boa porque as questões não fecham dezena (só unidade)
	question_val = i.strip('\t\n')[8:9]
	dic_q["question"] = question_val

	# aqui já começa a dar pau, seria melhor usar re	
	article_val = i.strip('\t\n')[10:15]
	dic_q["article_val"] = article_val

	questões_dic.append(dic_q)

print (questões_dic)
