addpath("Research/Repulsion_Effect/Experiments/circle_area_exp_ptb/RDCL_Functions/");
addpath("Research/Repulsion_Effect/Experiments/circle_area_exp_ptb/utility_functions/");

h1 = [8];
w1 = [11];
h2 = [14];
w2 = [17];
h3 = [20];
w3 = [23];

rects_order_tmp = RDCL_RandomizeTrials(3);
r1_index_tmp = rects_order_tmp(1);
r2_index_tmp = rects_order_tmp(2);
r3_index_tmp = rects_order_tmp(3);  

h_all_tmp = [h1 h2 h3];
w_all_tmp = [w1 w2 w3];

h1_tmp = h_all_tmp(r1_index_tmp);
h2_tmp = h_all_tmp(r2_index_tmp);
h3_tmp = h_all_tmp(r3_index_tmp);

w1_tmp = w_all_tmp(r1_index_tmp);
w2_tmp = w_all_tmp(r2_index_tmp);
w3_tmp = w_all_tmp(r3_index_tmp);

x = [5 0 4];

find(x>0);
str2double('1')
exist('1')