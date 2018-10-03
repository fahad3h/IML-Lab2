library(boot)
library(tree)
library(ggplot2)

# 3.1 Data import, reorder and Plot
set.seed(12345)
data = read.csv2("State.csv", header = TRUE)
data = data[order(data$MET),]
plot(EX ~ MET, data = data ,col="red", pch= 19)

# 3.2 Sit regression model
control_parameter = tree.control(nobs = nrow(data),minsize = 8)
                                   
fit_tree = tree(formula = EX ~ MET,data = data,control = control_parameter)
leave_fit = cv.tree(fit_tree)

op_tree = prune.tree(fit_tree,best = leave_fit$size[which.min(leave_fit$dev)])

prediction_val = predict(op_tree, newdata=data)

df = data.frame(x = data$MET, pred = prediction_val, ex_val = data$EX)
fit_original_plot = ggplot(df, aes(x, pred, ex_val)) + 
  geom_point(aes(x,ex_val), colour = "blue") +
  geom_point(aes(x, pred)) 
fit_original_plot

hist(residuals(op_tree))

# 3.3 Non-Paramatric Bootstrap
f_np = function(data,index){
  sample = data[index,]
  Ctrl = tree.control(nrow(sample), minsize = 8)
  fit = tree( EX ~ MET, data=sample, control = Ctrl)
  optimal_tree = prune.tree(fit, best= leave_fit$size[which.min(leave_fit$dev)]) 
  return(predict(optimal_tree, newdata=data))
}

np_bs = boot(data, statistic = f_np, R=1000)
conf_bound = envelope(np_bs,level=0.95)
predictions = predict(op_tree,data)

plot(np_bs)

df_1 = data.frame(ex_val = data$EX, x = data$MET, pred = predictions, upper=conf_bound$point[1,], lower=conf_bound$point[2,])
CB_plot = ggplot(df_1, aes(x, pred, ex_val)) + 
          geom_point(aes(x,ex_val), colour = "blue") +
          geom_point(aes(x, pred)) 
CB_plot

fig_data = data.frame(orig = data$EX, x=data$MET, pred=predictions, upper=conf_bound$point[1,], lower=conf_bound$point[2,])
fig = ggplot(fig_data, aes(x,predictions,upper,lower))
fig = fig +
  geom_point(aes(x, pred)) + 
  geom_point(aes(x, orig),colour="blue") 
  geom_line(aes(x,upper)) +
  geom_line(aes(x,lower))
print(fig)

# 3.4 Paramatric Bootstrap
parama_conf = function(data){
  controll = tree.control(nrow(data), minsize = 8)
  fit = tree( EX ~ MET, data=data, control = controll)
  op_tree = prune.tree(fit, best=leave_fit$size[which.min(leave_fit$dev)]) 
  return(predict(op_tree, newdata=data))
}

parama_predic = function(data){
  controll = tree.control(nrow(data), minsize = 8)
  fit = tree( EX ~ MET, data=data, control = controll)
  op_tree = prune.tree(fit, best=leave_fit$size[which.min(leave_fit$dev)]) 
  predictions = predict(op_tree, newdata=data)
  return(rnorm(nrow(data),predictions,sd(resid(fit))))
}

rnd = function(data, model){
  sample = data.frame(MET=data$MET, EX=data$EX)
  sample$EX = rnorm(nrow(data), predict(model,newdata=data),sd(resid(model)))
  return(sample)
}

set.seed(12345)
param_boot_conf = boot(data, statistic = parama_conf, R=1000, mle = op_tree, ran.gen = rnd, sim = "parametric")
confidence_bound_param = envelope(param_boot_conf, level=0.95)

plot(param_boot_conf)
 
set.seed(12345)
param_boot_pred = boot(data, statistic = parama_predic, R=1000, mle = op_tree, ran.gen = rnd, sim = "parametric")
prediction_bound_param = envelope(param_boot_pred, level=0.95)

plot(param_boot_pred)

predictions = predict(op_tree,data)
fig_data = data.frame(orig = data$EX, x=data$MET, pred=predictions, upper_c=confidence_bound_param$point[1,], lower_c=confidence_bound_param$point[2,], upper_p=prediction_bound_param$point[1,], lower_p=prediction_bound_param$point[2,])

para_plot = ggplot(fig_data, aes(orig,x,pred,upper_c,lower_c, upper_p, lower_p))
para_plot = para_plot +
  geom_point(aes(x, pred)) + 
  geom_point(aes(x, orig),colour="red") + 
  geom_line(aes(x,upper_c)) +
  geom_line(aes(x,lower_c)) +
  geom_line(aes(x,upper_p)) +
  geom_line(aes(x,lower_p))
print(para_plot)
