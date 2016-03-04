data = "/home/jodaiber/Dropbox/thesis/data"

.PHONY: evaluate start_moses

build:
	mvn clean install

start_moses:
	nohup /data/thesis/moses/mosesdecoder/mosesdecoder/bin/mosesserver -f /data/thesis/moses/mert-work/moses.ini &

###########################################################################
# Training
###########################################################################

train_mst: build
	mvn exec:java -pl mstparser -Dexec.mainClass="mstparser.DependencyParser" -Dexec.args="train train-file:$(data)/penn_depbank/train_with_universal_cpos.wsj model-name:eng.wsj.model training-k:5 loss-type:punc decode-type:proj test test-file:$(data)/penn_depbank/test_with_universal_cpos.wsj output-file:out.txt eval gold-file:$(data)/penn_depbank/test_with_universal_cpos.wsj format:CONLL create-forest:true ord:2"

train_mst_merge100: build
	cd models/merge100; mvn -f ../../pom.xml exec:java -pl mstparser -Dexec.mainClass="mstparser.DependencyParser" -Dexec.args="train train-file:$(data)/penn_depbank/train_merged_100 model-name:eng.merged100.model training-k:5 loss-type:punc decode-type:proj test test-file:$(data)/penn_depbank/test_with_universal_cpos.wsj output-file:out_merged100.txt eval gold-file:$(data)/penn_depbank/test_with_universal_cpos.wsj format:CONLL create-forest:true ord:2"

train_mst_merge250: build
	cd models/merge250; mvn -f ../../pom.xml exec:java -pl mstparser -Dexec.mainClass="mstparser.DependencyParser" -Dexec.args="train train-file:$(data)/penn_depbank/train_merged_250 model-name:eng.merged250.model training-k:5 loss-type:punc decode-type:proj test test-file:$(data)/penn_depbank/test_with_universal_cpos.wsj output-file:out_merged250.txt eval gold-file:$(data)/penn_depbank/test_with_universal_cpos.wsj format:CONLL create-forest:true ord:2"





###########################################################################
# Evaluation
###########################################################################

evaluate: build evaluate/foster_aligned evaluate/daiber evaluate/foster_full_aligned evaluate/daiber_full evaluate/foster

evaluate/foster:
	mkdir -p $@
	mvn exec:java -pl eval -Dexec.mainClass="ithaka.eval.EvaluateParsing" -Dexec.args="-o $@ opts.txt $(data)/Foster2011/twitterdev_tokens $(data)/Foster2011/conll/dev.foster.with_universal_cpos" >$@/out 2>&1

evaluate/foster_aligned:
	mkdir -p $@
	mvn exec:java -pl eval -Dexec.mainClass="ithaka.eval.EvaluateParsing" -Dexec.args="-a $(data)/Foster2011/conll/twitterdev_tokens_normalized -o $@ opts.txt $(data)/Foster2011/twitterdev_tokens $(data)/Foster2011/conll/dev.foster.with_universal_cpos " >$@/out 2>&1

evaluate/foster_full_aligned:
	mkdir -p $@
	mvn exec:java -pl eval -Dexec.mainClass="ithaka.eval.EvaluateParsing" -Dexec.args="-a $(data)/Foster2011/conll/twitterfull_tokens_normalized -o $@ opts.txt $(data)/Foster2011/conll/twitterfull_tokens $(data)/Foster2011/conll/full.foster.with_universal_cpos " >$@/out 2>&1

evaluate/daiber:
	mkdir -p $@
	mvn exec:java -pl eval -Dexec.mainClass="ithaka.eval.EvaluateParsing" -Dexec.args="-o $@ opts.txt -a $(data)/twitter_testset/v1/full/dev_normalized $(data)/twitter_testset/v1/full/dev_tokens $(data)/twitter_testset/v1/full/dev_conll" >$@/out 2>&1

evaluate/daiber_test:
	mkdir -p $@
	mvn exec:java -pl eval -Dexec.mainClass="ithaka.eval.EvaluateParsing" -Dexec.args="-o $@ opts.txt -a $(data)/twitter_testset/v1/full/test_normalized $(data)/twitter_testset/v1/full/test_tokens $(data)/twitter_testset/v1/full/test_conll" >$@/out 2>&1

evaluate/daiber_full:
	mkdir -p $@
	mvn exec:java -pl eval -Dexec.mainClass="ithaka.eval.EvaluateParsing" -Dexec.args="-a $(data)/twitter_testset/v1/full/devtest_normalized opts.txt $(data)/twitter_testset/v1/full/devtest_tokens $(data)/twitter_testset/v1/full/devtest_conll" >$@/out 2>&1

evaluate/daiber_test_100:
	mkdir -p $@
	mvn exec:java -pl eval -Dexec.mainClass="ithaka.eval.EvaluateParsing" -Dexec.args="-o $@ models/merge100/opts.txt -a $(data)/twitter_testset/v1/full/test_normalized $(data)/twitter_testset/v1/full/test_tokens $(data)/twitter_testset/v1/full/test_conll" >$@/out 2>&1

evaluate/daiber_test_250:
	mkdir -p $@
	mvn exec:java -pl eval -Dexec.mainClass="ithaka.eval.EvaluateParsing" -Dexec.args="-o $@ models/merge250/opts.txt -a $(data)/twitter_testset/v1/full/test_normalized $(data)/twitter_testset/v1/full/test_tokens $(data)/twitter_testset/v1/full/test_conll" >$@/out 2>&1



###########################################################################
# Error analysis
###########################################################################

error_analyis: build
	mvn exec:java -pl eval -Dexec.mainClass="ithaka.eval.ErrorAnalysis" -Dexec.args="opts.txt $(data)/twitter_testset/v1/full/dev_tokens $(data)/twitter_testset/v1/full/dev_conll $(data)/twitter_testset/v1/full/dev_normalized"
