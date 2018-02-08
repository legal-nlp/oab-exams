
# Simulations exam of the Brazilian Bar Exam (OAB) made by FGV DIREITO RIO.

In order to make the process of cleaning and formated data more efficient, two
scritps written in Python were used: clean\_data\_simulated.py and
insert\_answer\_simulated.py. However, this process is not fully automatic.
Hence, some manual changes are needed.

The steps of the process used is:

1. All files with the questions were converted from docx to txt using the tool
   docx2txt

2. All files with the answers were converted from docx to odt. And, later,
   converted to txt with the tool odt2txt

3. We would take the file on simulated/raw, for instance, 2011-5.txt, and delete
   the first lines with instructions and the last lines with the student version
   of the answer-sheet.

4. Now, we would run the script clean\_data\_simulated.py. This would insert
   this format:


5. After this script, we would manually look for erros and fix things that do
   not appear as a pattern. In addition, we would add the AREA value, e. g.,
   AREA _CONSTITUTIONAL_

6. Now, we would treat the answers. First, geting the files with answer-sheet in
   a docx format, converting it to odt and, finally, to txt. This approach was
   better and avoid the losing of data.

7. Having the answers in txt, we would run insert\_answer\_simulated.py 

8. Finally, we would check if the answers were correctly inserted, having the
   original answer-sheet in docx as the "control group".

8. Finally, we would check if the answers were correctly inserted, having the
   original answer-sheet in docx as the "control group".

8. Finally, we would check if the answers were correctly inserted, having the
   original answer-sheet in docx as the "control group".

8. Finally, we would check if the answers were correctly inserted, having the
   original answer-sheet in docx as the "control group".

8. Finally, we would check if the answers were correctly inserted, having the
   original answer-sheet in docx as the "control group".

8. Finally, we would check if the answers were correctly inserted, having the
   original answer-sheet in docx as the "control group".

8. Finally, we would check if the answers were correctly inserted, having the
   original answer-sheet in docx as the "control group".

8. Finally, we would check if the answers were correctly inserted, having the
   original answer-sheet in docx as the "control group".


