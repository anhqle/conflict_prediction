
boost_in_pred <- predict.gbm(boosted_tree, traindata,
                             n.trees=boosted_tree$gbm.call$best.trees, type="response")
boost_out_pred <- predict.gbm(boosted_tree, testdata,
                              n.trees=boosted_tree$gbm.call$best.trees, type="response")
