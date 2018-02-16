# encoding: utf-8

import fileinput

# ask for the txt file with the answers

answers_txt = input("insert the name of the file with the answer-sheet in txt: ")

with open(answers_txt) as f:
    
    mylist = f.read().splitlines()

answer_list = []

for string in mylist:
    
    if string=="A" or string=="B" or string=="C" or string=="D":

        answer_list.append(string+") ")

as_len = len(answer_list)

print (answer_list, as_len)

if as_len != 70:
    
    exit("did not get all the answers from the answer-sheet, exit the program")

iter_num = -1

original_file = input("insert the name of the raw file, still missing the answer-sheet: ")

def convert_to_string(lista):
    
    new_list = []

    for i in lista:

        new_list.append(str(i))

    return new_list 

zero_to_nine = list(range(0,10))

zero_to_nine = convert_to_string(zero_to_nine)

ten_to_eighty = list(range(10,81))

ten_to_eighty = convert_to_string(ten_to_eighty)


with open(original_file, 'r') as input_file, open('output_file.txt', 'w') as output_file:
    
    for line in input_file:
        
        # print (answer_list[iter_num])
        
        if iter_num==as_len:

            break
       
        elif line[:3]==answer_list[iter_num]:
 
            string = line
            new_string = string[:1]+":CORRECT)"+string[2:]

            output_file.write(new_string)
            
            # print (line) 
            # print (new_string)

        elif line[:7]=="OPTIONS":
        
            iter_num += 1
            
            output_file.write(line)
        
        elif (line[0] in zero_to_nine) and (line[1]=="."):
        
            deleted_num_point = line[0]+line[1]

            line = line.replace(deleted_num_point,"")

            output_file.write(line)


        elif (line[:2] in ten_to_eighty) and (line[2]=="."):

            deleted_num_point = line[0]+line[1]+line[2]

            line = line.replace(deleted_num_point,"")

            output_file.write(line)

        else:

            # print(line)
            output_file.write(line)



