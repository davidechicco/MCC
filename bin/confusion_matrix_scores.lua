SCORE_UNDEF = -2


-- from sam_lie
-- Compatible with Lua 5.0 and 5.1.
-- Disclaimer : use at own risk especially for hedge fund reports :-)

---============================================================
-- add comma to separate thousands
-- 
function comma_value(amount)
  local formatted = amount
  while true do  
    formatted, k = string.gsub(formatted, "^(-?%d+)(%d%d%d)", '%1,%2')
    if (k==0) then
      break
    end
  end
  return formatted
end

-- round a real value
function round(num, idp)
  local mult = 10^(idp or 0)
  return math.floor(num * mult + 0.5) / mult
end

-- Function that reads a value and returns the string of the signed value
function signedValueFunction(value)
  
      local value = tonumber(value);
      --print("value = "..value);
      local charPlus = ""
      if tonumber(value) >= 0 then charPlus = "+"; end
      local outputString = charPlus..""..tostring(round(value,2));
      --print("outputString = "..outputString);
      
      return tostring(outputString);
end



require 'optim'


-- function confusion_matrix_scores(tp, tn, fp, tn)
function confusion_matrix_scores(tp, tn, fp, tn)
  
    print("\n\n~ : ~ : ~ : ~ : ~ : ~ : ~ : ~ : ~ : ~")
    
    print("TP = "..tp)
    print("FN = "..fn.." \n")
    
    print("TN = "..tn)
    print("FP = "..fp.." \n")

    local tot_pos = (tp+fn)
    local tot_neg = (tn+fp)
    local tot_ele = (tot_pos+tot_neg)
    io.write("#total elements = "..tot_ele.."\n\n")
    io.write("#total positives = "..tot_pos.."\n")
    io.write("#total negatives = "..tot_neg.."\n")

    local tot_pos_perc = tot_pos / (tot_pos+tot_neg)
    local tot_neg_perc = tot_neg / (tot_pos+tot_neg)
    io.write("total positives percentage = "..tot_pos.."%\n")
    io.write("total negatives percentage = "..tot_neg.."%\n\n")

    io.flush()

    if tot_pos_perc > tot_neg_perc then print("This dataset is POSITIVELY IMbalanced\n")
    elseif tot_neg_perc > tot_pos_perc then print("This dataset is NEGATIVELY IMbalanced\n")
    elseif tot_neg_perc == tot_pos_perc then print("This dataset is BALANCED\n")
    end


    accuracy = (tp + tn)/(tp + tn +fn + fp)
    -- ######## Line too long (109 chars) ######## :
    print("accuracy = "..round(accuracy,2).. " = (tp + tn)/(tp + tn +fn + fp) \t\t [worst = 0, best =1]");

    local f1_score = SCORE_UNDEF
    if (tp+fp+fn)>0 then 
    f1_score = (2*tp) / (2*tp+fp+fn)
    -- ######## Line too long (95 chars) ######## :
    print("f1_score = "..round(f1_score,2).." = (2*tp) / (2*tp+fp+fn) \t\t [worst = 0, best = 1]");
    else
    print("f1_score CANNOT be computed because (tp+fp+fn)==0")
    end


    local upperMCC = (tp*tn) - (fp*fn)
    local innerSquare = (tp+fp)*(tp+fn)*(tn+fp)*(tn+fn)
    local lowerMCC = math.sqrt(innerSquare)

    if lowerMCC>0 then MatthewsCC = upperMCC/lowerMCC
    else
    MatthewsCC = SCORE_UNDEF
    end
    local signedMCC = signedValueFunction(MatthewsCC);

    -- io.write("\n\n\nMCC = "..signedMCC.." ");
    -- io.flush();

    if MatthewsCC ~= SCORE_UNDEF then 
      io.write("\nMatthews correlation coefficient = "..signedMCC.."\t");
      print("\t\t [worst = 0, best = 1]\n")
    else 
      print("Matthews correlation coefficient = NOT computable");	
    end

    if accuracy >= 0.5 and f1_score >= 0.5 and MatthewsCC < 0.5 then
      print("[MCC != Acc =! F1] The MCC is in disagreement with accuracy and f1_score")
    elseif (accuracy >= 0.5 and f1_score >= 0.5 and MatthewsCC >= 0.25) or (accuracy < 0.5 and f1_score < 0.5 and MatthewsCC < 0.25) then
	print("[MCC == Acc == F1] The MCC is concordant with accuracy and f1_score")
    elseif (accuracy >= 0.5 and f1_score < 0.5 and MatthewsCC < 0.25) or (accuracy < 0.5 and f1_score >= 0.5 and MatthewsCC >= 0.25) then
	print("[MCC == F1 != Acc] The MCC is concordant with f1_score and in disagreement with accuracy")
    elseif (accuracy < 0.5 and f1_score >= 0.5 and MatthewsCC < 0.25) or (accuracy >= 0.5 and f1_score < 0.5 and MatthewsCC >= 0.25) then
	print("[MCC == Acc != F1] The MCC is concordant with accuracy and in disagreement with f1_score")
    end

    -- local false_discovery_rate = SCORE_UNDEF
    -- if (fp+tp)>0 then 
    -- false_discovery_rate = fp / (fp + tp)
    -- -- ######## Line too long (111 chars) ######## :
    -- print("\nfalse_discovery_rate = "..round(false_discovery_rate,2).." = fp / (fp + tp) \t\t [worst = 1, best = 0]")
    -- end
    -- 
    -- local precision = SCORE_UNDEF
    -- if (tp+fp)>0 then
    -- precision = tp/(tp+fp)
    -- -- ######## Line too long (89 chars) ######## :
    -- print("precision = "..round(precision,2).." = tp / (tp + fp) \t\t [worst = 0, best = 1]")
    -- end
    -- 
    -- local recall = SCORE_UNDEF
    -- if (tp+fn)>0 then
    -- recall = tp/(tp+fn)
    -- -- ######## Line too long (83 chars) ######## :
    -- print("recall = "..round(recall,2).." = tp / (tp + fn) \t\t [worst = 0, best = 1]")
    -- end

    io.write("\n")
    
    print("~ : ~ : ~ : ~ : ~ : ~ : ~ : ~ : ~ : ~")
end


tp = 90
fn = 1

tn = 0
fp = 9

confusion_matrix_scores(tp, tn, fp, tn)


tp = 80
fn = 10

tn = 1
fp = 9

confusion_matrix_scores(tp, tn, fp, tn)


tp = 10
fn = 80

tn = 9
fp = 1

confusion_matrix_scores(tp, tn, fp, tn)

tp = 1
fn = 9

tn = 89
fp = 1

confusion_matrix_scores(tp, tn, fp, tn)


tp = 47
fn = 3

tn = 5
fp = 45

confusion_matrix_scores(tp, tn, fp, tn)