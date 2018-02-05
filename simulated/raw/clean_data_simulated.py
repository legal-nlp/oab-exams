# encoding: utf-8
#python3

""" 
O objetivo desse arquivo é tornar mais automático o processo de formatação e
limpeza dos dados das provas simuladas pela FGV DIREITO RIO. Os arquivos de
raw/simulated precisam ter o mesmo formato que os arquivos em raw/official.
Esse processo não é totalmente automático, ainda será necessário ajustar
manualmente os corner cases que esse script não resolve, como primeiro e último,
inserir área e corrigir erros pontuais que não possuem um padrão.
Posteriormente, será feito um outro script apenas para introduzir o gabarito.
"""
# como boa prática vou ler as informações em um arquivo e escrever em outro
file_object = open("2011-5-original.txt", "r")

# iterar sobre cada linha do arquivo que estou lendo
for line in file_object:
    
    """
    O comando lstrip() remove espaços no início da linha até
    encontrar um caracter, como:
    " Example"    -> "Example"
    "  Example  " -> "Example  "
    "    Example" -> "Example"
    """
    line = line.lstrip()

    # as alternativas são seguidas de ")" no official/raw e não "."    
    # além disso, aproveitar a alternativa "A" para inserir o marcador OPTIONS
    if line[:3]=="A. ":
            
        line = line.replace("A. ","A) ")
        
        line = "OPTIONS \n" + "\n" + line

    if line[:3]=="B. ":
            
        line = line.replace("B. ","B) ")

    if line[:3]=="C. ":
            
        line = line.replace("C. ","C) ")

    """
    As alternativas B e C foram arrumadas em relação ao "X) .".  No caso da
    alternativa D, vou usá-la para inserir outros marcadores como "---" e "ENUM
    Questão".  Atenção, vai faltar esse marcador na primeira questão e vai
    sobrar no final um marcador solto. Vou, manualmente, acrescentar a primeira e
    tirar o que sobrar.
    """
    if line[:3]=="D. ":
            
        line = line.replace("D. ","D) ")
        
        line = line  + "\n" + "---\n" + "ENUM Questão \n"  + "\n" + "AREA  "  
 
    print (line)

"""
para rodar o arquivo eu usei o comando no vim:

:w |! python clean_data_simulated.py >> output_test.txt

como dito anteriormente, leio em um arquivo e escrevo no outro
"""
