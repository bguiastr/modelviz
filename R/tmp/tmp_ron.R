# just to test out some things
library(modelviz)

test <- import_qmd_info(dir='~/git/nonmem_examples/Hands_onA/', run=101)
des_block <- "
DADT(1) = -KA*A(1)
DADT(2) = KA*A(1) - KEL*A(2)
"
qmd(test, des_block = des_block)


node <- DiagrammeR::create_nodes(nodes    = c('A1', 'A2'),
                                 label    = c('A1', 'A2'),
                                 rank     = c(1, 1),
                                 prm      = c(10, 10, recursive=TRUE),
                                 rse      = c(.1, .1, recursive=TRUE))

test <- import_qmd_info(dir='~/git/nonmem_examples/Hands_onA/', run=101)
qmd(test)
