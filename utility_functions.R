get_att_specs <- function(d,data_shape="long",data="circle"){
  # browser()
  dd <- d %>%
    filter(str_detect(effect,"attraction")) %>%
    mutate(
      d_identity=case_when(
        rect1_area < rect2_area & rect1_area < rect3_area ~ 1,
        rect2_area < rect1_area & rect2_area < rect3_area ~ 2,
        rect3_area < rect1_area & rect3_area < rect2_area ~ 3,
      ),
      t_identity = case_when(
        set=="a-b-da" & d_identity == 1 & h2 < w2 ~ 3,
        set=="a-b-da" & d_identity == 1 & h3 < w3 ~ 2,
        set=="a-b-da" & d_identity == 2 & h3 < w3 ~ 1,
        set=="a-b-da" & d_identity == 2 & h1 < w1 ~ 3,
        set=="a-b-da" & d_identity == 3 & h1 < w1 ~ 2,
        set=="a-b-da" & d_identity == 3 & h2 < w2 ~ 1,
        set=="a-b-db" & d_identity == 1 & h2 > w2 ~ 3,
        set=="a-b-db" & d_identity == 1 & h3 > w3 ~ 2,
        set=="a-b-db" & d_identity == 2 & h3 > w3 ~ 1,
        set=="a-b-db" & d_identity == 2 & h1 > w1 ~ 3,
        set=="a-b-db" & d_identity == 3 & h1 > w1 ~ 2,
        set=="a-b-db" & d_identity == 3 & h2 > w2 ~ 1
      ),
      c_identity=case_when(
        d_identity==1 & t_identity==2 ~ 3,
        d_identity==1 & t_identity==3 ~ 2,
        d_identity==2 & t_identity==1 ~ 3,
        d_identity==2 & t_identity==3 ~ 1,
        d_identity==3 & t_identity==1 ~ 2,
        d_identity==3 & t_identity==2 ~ 1,
      ),
      disp_cond=factor(disp_cond,levels=c("triangle","horizontal")))
  if(stringr::str_detect(data,"circle")){
    # browser()
    dd <- dd %>% mutate(
      d_area=case_when(
        d_identity==1~circle1_area,
        d_identity==2~circle2_area,
        d_identity==3~circle3_area,
      ),
      c_area=case_when(
        c_identity==1~circle1_area,
        c_identity==2~circle2_area,
        c_identity==3~circle3_area,
      ),
      t_area=case_when(
        t_identity==1~circle1_area,
        t_identity==2~circle2_area,
        t_identity==3~circle3_area,
      )) %>%
      select(-c(contains("circle"),contains("rect")) 
    )
    if(str_detect(data_shape,"wide")){
      return(rename_with(dd, ~str_remove(.x, "_area$")))
    }else{
      dd %>%
        pivot_longer(contains("_area"),values_to = "a") %>%
        separate(name, into = c("stim","drop")) 
    }
  }else if(str_detect(data,"choice")){
    ddd <- dd %>%
      mutate(choice_tdc=case_when(
        choice==d_identity~"decoy",
        choice==t_identity~"target",
        choice==c_identity~"competitor"
      ),
      choice_abd=case_when(
        choice==d_identity~"d",
        choice==t_identity & str_detect(set,"a-b-da")~"a",
        choice==t_identity & str_detect(set,"a-b-db")~"b",
        choice==c_identity & str_detect(set,"a-b-da")~"b",
        choice==c_identity & str_detect(set,"a-b-db")~"a",
      )) %>%
      select(-contains("identity|area"))
    return(ddd)
  }
}

drop_leading_zero <- function(l){
  str_replace(l, '0(?=.)', '')
}
