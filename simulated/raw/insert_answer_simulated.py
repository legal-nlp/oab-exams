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

# print (answer_list)

iter_num = -1

original_file = input("insert the name of the raw file, still missing the answer-sheet: ")

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

        else:

            # print(line)
            output_file.write(line)


