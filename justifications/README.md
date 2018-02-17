# fundamento-respostas.pdf

contains the original justifications for 30 questions from the ethics section of
the OAB exam.

# relatorio-wn-oabexams.docx

a report on how well the Brazilian WordNet handles legal terms.

# justify-constitutional.yaml and justify-ethics.yaml

contains the justification for some questions from the OAB exam in
machine-readable format.

there are two files, one about constitutional questions and the other with
questions on ethics.

a justification is the article, subsection and law which justifies the correct
answer.

the format uses yaml with the following structure:

``` 
exam: 

question: 

urn: 

article: 

comment: 

```

- `exam`(YYYY-[0-9]{2}[a-z]{1}: the year and the edition of the exam, plus an
  optional letter if its an exam reapplication.

- `question` ([0-9]+): an integer.

- `urn`: the URN of the legal norm that justifies the correct answer (see [LexML
  URN](http://projeto.lexml.gov.br/documentacao/Parte-2-LexML-URN.pdf) and [XML
  Schema](http://projeto.lexml.gov.br/documentacao/Parte-3-XML-Schema.pdf)).

- `article` (art[0-9]+): if the article has a number and a letter (as in the OAB
  regimento) use (art[0-9]+-[0-9]+) as LexML  recommends.

- `comments`: any string without tabs (`\t`).

