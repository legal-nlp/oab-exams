# justify.txt

contains the justification for some questions from the OAB exam in
machine-readable format.

a justification is the article and law which justifies the correct
answer.

the format is as follows (fields are separated by tabs):

```
year-edition	question_number	article_number	justification_URN	comments
```

- `year-edition`(YYYY-[0-9]{2}[a-z]{1}: the year and the edition of
  the exam, plus an optional letter if its an exam reapplication.

- `question_number` ([0-9]+): an integer.

- `article_number` (art[0-9]+): if the article has a number and a
  letter (as in the OAB regimento) use (art[0-9]+-[0-9]+) as LexML
  recommends.

- `justification_URN`: the URN of the legal norm that justifies the
  correct answer (see [LexML URN](http://projeto.lexml.gov.br/documentacao/Parte-2-LexML-URN.pdf) and [XML Schema](http://projeto.lexml.gov.br/documentacao/Parte-3-XML-Schema.pdf)).

- `comments`: any string without tabs (`\t`).

# fundamento-respostas.tex

contains the original justifications for 30 questions from the ethics
section of the OAB exam.

# relatorio-wn-oabexams.docx

a report on how well the Brazilian WordNet handles legal terms.
