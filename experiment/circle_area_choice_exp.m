% Spring 2024
% Circle-adjustment && choice CE exp
% Sean Conway
function circle_area_choice_exp
   
    % PTB SETUP %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    more off;
    global screen_ptr;
    debug=false;
    if debug
      PsychDebugWindowConfiguration;
    endif

    % ADD NECESSARY FILE PATHS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    % FUNCTIONS
    addpath('./RDCL_Functions');    
    addpath('./utility_functions');
    
    % TRIAL PARAMS
    addpath('./circle_trial_params');
    addpath('./choice_trial_params');
    addpath('./comp_params');

    % DATA PATHS
    circle_destination_folder = './data/circle_area';
    choice_destination_folder = './data/choice';
    addpath(circle_destination_folder);
    addpath(choice_destination_folder);
    
    computer_num = 2;%dlmread('computer_num.txt');

    % MORE SETUP %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    %KB NAME
    KbName('UnifyKeyNames');
    
    if ~debug
      % Dummy calls
      RDCL_DummyCalls();
      % minimum time before participants can hit the 'submit' button
      submit_min=3.5;
    else
      submit_min=0;
    endif
    
    % seed rng
    RDCL_SeedRandomNumbers();
      
    % EXPERIMENT SETTINGS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    % min circle area && radius
    min_circle_rad = 5;
    min_circle_area = area_from_rad(min_circle_rad);

    % NUMBER OF BLOCKS 
    n_circle_blocks = 4;
    n_choice_blocks = 4;
    
    % number of calibration trials per circle block
    n_calib = 3;

    % number of practice trials for circle and choice tasks
    n_circle_prac = 3;
    n_choice_prac = 3;

    %% max vertical jitter
    vjit = 15; 

    % COLORS
    gray = [87 88 91];
    white = [255 255 255];
    black = [0 0 0];
    red = [255 0 0];
    rect_fill = gray;
    circle_fill = gray;

    % rect border thickness
    submit_rect_thick = 1;
  
    % rectangle distance
    rect_dist = .05;
    
    % lag time between adjustments
    lag_time = .008;
    
   

    %%%%%%% GET SUBJECT NUMBER %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    % Prompt text
    prompt = ('Please enter the participant number:');
    
    % Get Experimenter input
    answer = inputdlg(prompt);
    sub_n = deal(answer{:});
    sub_n = num2str(sub_n);

    % OUTPUT FILE NAMES
    % separate data file for circle and choice responses
    circle_output_name = ['circle_area_sub' sub_n '.csv'];
    choice_output_name = ['choice_sub' sub_n '.csv'];      

    % Check to see if subject number has been used
    while exist(circle_output_name) | exist(choice_output_name)
      sub_n_error = ('That participant number has already been used. Please double check the runsheet and enter a new number.');
      error_message = inputdlg(sub_n_error);
      sub_n = deal(error_message{:});
      sub_n = num2str(sub_n);
      circle_output_name = ['circle_area_sub' sub_n '.csv'];
      choice_output_name = ['choice_sub' sub_n '.csv'];
    end  

    % IMPORTANT %
    % % ASSIGN SUBJECT TO CONDITION
    % odd subject # means horizontal, even subject # means triangle
    if rem(str2double(sub_n),2)>0
      disp_cond = "horizontal";
    else
      disp_cond = "triangle";
    endif 
    
    % START EXPERIMENT %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    % Open Screen  
    [screen_ptr s_rect] = Screen('OpenWindow', 0, 0, [], 32, 2);
    
    % FIND SCREEN CENTER
    s_middle_x = s_rect(3)*.5;
    s_middle_y = s_rect(4)*.5;
 % rectangle distance in pixels
    rect_dist_px = round(s_middle_x * rect_dist);
    % GET CONSENT %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
    if ~debug
      % SHOW CONSENT FORM
     cons_info = imfinfo('consent.png');
     cons_width = cons_info.Width;
     cons_height = cons_info.Height;
     %Get the image for instructions
     cons= imread('consent.png');
     cons_scale = .4;
     cons_rect = [s_middle_x-round(cons_scale*cons_width/1.5) 
                  s_middle_y-round(cons_scale*cons_height/1.5) 
                  s_middle_x+round(cons_scale*cons_width/1.5) 
                  s_middle_y+round(cons_scale*cons_height/1.5)];
     
     % Put the image into the buffer      
     agree_rect = CenterRectOnPoint([0 0 100 60], s_middle_x-300, cons_rect(4)+75);
     not_agree_rect = CenterRectOnPoint([0 0 300 60], s_middle_x+300, cons_rect(4)+75);      
      
     %Put the image into the buffer
     Screen('FillRect', screen_ptr, white);
     Screen('PutImage', screen_ptr, cons, cons_rect);
     Screen('FrameRect', screen_ptr, black, agree_rect, submit_rect_thick);
     Screen('FrameRect', screen_ptr, black, not_agree_rect, submit_rect_thick);
     RDCL_DrawText(s_middle_x-300, cons_rect(4)+65, "I agree", 'Col',black);
     RDCL_DrawText(s_middle_x+300, cons_rect(4)+65, "I do not agree", 'Col',black);
     Screen('Flip', screen_ptr); 

     % CHECK IF THEY ACCEPTED OR DECLINED TO PARTICIPATE
     deciding=true;
     while deciding
     [clicks click_x click_y] = GetClicks(screen_ptr);
      if click_x > agree_rect(1) && click_x < agree_rect(3) && click_y > agree_rect(2) && click_y < agree_rect(4)
        deciding=false;
        declined=false;
      elseif  click_x > not_agree_rect(1) && click_x < not_agree_rect(3) && click_y > not_agree_rect(2) && click_y < not_agree_rect(4)
        deciding=false;
        declined=true;
      endif
     endwhile  
     if declined
       sca
     endif     
     
     % Clear Screen
     Screen('FillRect', screen_ptr, white);
     Screen('Flip', screen_ptr); 
     WaitSecs(1);
    endif
     
    % STIMULUS SETTINGS FOR EXPERIMENT %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    % submit button for circle trials
    submit_rect_c = [s_rect(3)*.75 s_rect(4)*.9];
    submit_rect = CenterRectOnPoint([0 0 200 100], submit_rect_c(1), submit_rect_c(2));

    % quadrant locs for drawing rectangles and circles
    rect_quadrant = [0 s_middle_y s_middle_x s_rect(4)];
    circle_quadrant_width = s_middle_x;
    circle_square_max_length = round ((s_middle_x-2*rect_dist_px)/3);
    circle_quadrant = [s_middle_x 25 circle_quadrant_width circle_square_max_length*2+rect_dist_px];
    circle_quadrant_height = round(circle_quadrant(4)-circle_quadrant(2));
    circle_quadrant_center = [round(circle_quadrant(1)+circle_quadrant_width*.5)
                              round(circle_quadrant(2)+circle_quadrant_height*.5)];
    % maximum size a circle can be
    max_circle_area = round(area_from_rad(.5*circle_square_max_length));

    % actual "rectangle" where rectangles go
    rectangle_box_c = [round(.26*s_rect(3)) round(.74*s_rect(4))];
    rectangle_box = CenterRectOnPoint(rect_quadrant, rectangle_box_c(1), rectangle_box_c(2));

    % width & height of the rectangle box
    rectangle_box_w = rectangle_box(3)-rectangle_box(1);
    rectangle_box_h = rectangle_box(4)-rectangle_box(2);

    % Create/open circle area output file
    fp_circle = fopen(fullfile(circle_destination_folder,circle_output_name),'w');     
    % Write to circle area output file
    fprintf(fp_circle, 'sub_n, computer_n, disp_cond, effect, set, distance, diag, block_n, trial_n, h1, w1, h2, w2, h3, w3, circle1_area, circle1_rad, circle2_area, circle2_rad, circle3_area, circle3_rad, rt \n');
    
    % READ IN EXPERIMENTAL CIRCLE TRIAL PARAMETERS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
    % NUMERIC PARAMETERS
    % read in numeric parameters
    h1 = dlmread("circle_trial_params/h1.txt");
    h2 = dlmread("circle_trial_params/h2.txt");
    h3 = dlmread("circle_trial_params/h3.txt");
    w1 = dlmread("circle_trial_params/w1.txt");
    w2 = dlmread("circle_trial_params/w2.txt");
    w3 = dlmread("circle_trial_params/w3.txt");
    distance = dlmread("circle_trial_params/distance.txt");
    diag = dlmread("circle_trial_params/diag.txt");

    % diagonal ranges
    d3_r = 120:195;
    d2_r = 90:165;
    d1_r = 60:135;

    % diagonal intercepts
    d1_int = 195;
    d2_int = 255;
    d3_int = 315;

    % minimum and maximum stimulus value (for filler trials)
    min_stim = 56;
    max_stim = 195;

    % STRING PARAMETERS
    % read in string parameters
    sets = read_params("circle_trial_params/set.txt","%s");
    effect = read_params("circle_trial_params/effect.txt","%s");

    % DETERMINE N TRIALS FOR CIRCLE TASK
    n_circle_trials=numel(h1);

    % empty array for keeping track of ppt error
    circle_error = [];

    % CIRCLE INSTRUCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    Screen('FillRect', screen_ptr, white);
    RDCL_DrawText(s_middle_x, s_middle_y-300, "Welcome! Thank you for participating.", 'Col', black);
    RDCL_DrawText(s_middle_x, s_middle_y-250, "This experiment will take place in two phases. In the first phase, you will be making judgments about the area of rectangles.", 'Col', black);
    RDCL_DrawText(s_middle_x, s_middle_y-200, "We will explain the second phase after you complete the first phase.", 'Col', black);
    RDCL_DrawText(s_middle_x, s_middle_y-150, "Press 'space' to continue the instructions.", 'Col', black);
    Screen('Flip', screen_ptr); 
    RDCL_GetResponse({{'space', 0}});

    RDCL_DrawText(s_middle_x, s_middle_y-300, "On each trial you will see 3 rectangles and 3 circles as shown below.", 'Col', black);
    Screen('FillRect', screen_ptr, rect_fill, [s_middle_x-200 s_middle_y-250 s_middle_x-100 s_middle_y-100]);
    Screen('FillRect', screen_ptr, rect_fill, [s_middle_x s_middle_y-250 s_middle_x+120 s_middle_y-50]);
    Screen('FillRect', screen_ptr, rect_fill, [s_middle_x+200 s_middle_y-250 s_middle_x+300 s_middle_y-75]);
    RDCL_DrawText(s_middle_x-150, s_middle_y-25, "1", 'Col', black);
    RDCL_DrawText(s_middle_x+60, s_middle_y-25, "2", 'Col', black);
    RDCL_DrawText(s_middle_x+250, s_middle_y-25, "3", 'Col', black);

    Screen('FillOval', screen_ptr, rect_fill, [s_middle_x-200 s_middle_y+20 s_middle_x-100 s_middle_y+120]);
    Screen('FillOval', screen_ptr, rect_fill, [s_middle_x s_middle_y+20 s_middle_x+100 s_middle_y+120]);
    Screen('FillOval', screen_ptr, rect_fill, [s_middle_x+200 s_middle_y+20 s_middle_x+300 s_middle_y+120]);
    RDCL_DrawText(s_middle_x-150, s_middle_y+140, "1", 'Col', black);
    RDCL_DrawText(s_middle_x+60, s_middle_y+140, "2", 'Col', black);
    RDCL_DrawText(s_middle_x+250, s_middle_y+140, "3", 'Col', black);
    RDCL_DrawText(s_middle_x, s_middle_y+200, "The rectangles and circles are labeled '1', '2', and '3'. Rectangle 1 corresponds to circle 1, and so on.", 'Col', black);
    RDCL_DrawText(s_middle_x, s_middle_y+250, "Press 'space' to continue the instructions", 'Col', black);
    Screen('Flip', screen_ptr); 
    WaitSecs(1.5);
    RDCL_GetResponse({{'space', 0}});

    RDCL_DrawText(s_middle_x, s_middle_y-300, "Your job is to adjust each circle until it has the same area as the corresponding rectangle.", 'Col', black);
    Screen('FillRect', screen_ptr, rect_fill, [s_middle_x-200 s_middle_y-250 s_middle_x-100 s_middle_y-100]);
    Screen('FillRect', screen_ptr, rect_fill, [s_middle_x s_middle_y-250 s_middle_x+120 s_middle_y-50]);
    Screen('FillRect', screen_ptr, rect_fill, [s_middle_x+200 s_middle_y-250 s_middle_x+300 s_middle_y-75]);
    RDCL_DrawText(s_middle_x-150, s_middle_y-25, "1", 'Col', black);
    RDCL_DrawText(s_middle_x+60, s_middle_y-25, "2", 'Col', black);
    RDCL_DrawText(s_middle_x+250, s_middle_y-25, "3", 'Col', black);

    Screen('FillOval', screen_ptr, rect_fill, [s_middle_x-200 s_middle_y+20 s_middle_x-100 s_middle_y+120]);
    Screen('FillOval', screen_ptr, rect_fill, [s_middle_x s_middle_y+20 s_middle_x+100 s_middle_y+120]);
    Screen('FillOval', screen_ptr, rect_fill, [s_middle_x+200 s_middle_y+20 s_middle_x+300 s_middle_y+120]);
    Screen('FillOval', screen_ptr, rect_fill, [s_middle_x+200 s_middle_y+20 s_middle_x+300 s_middle_y+120]);
    RDCL_DrawText(s_middle_x-150, s_middle_y+140, "1", 'Col', black);
    RDCL_DrawText(s_middle_x+60, s_middle_y+140, "2", 'Col', black);
    RDCL_DrawText(s_middle_x+250, s_middle_y+140, "3", 'Col', black);
    RDCL_DrawText(s_middle_x, s_middle_y+200, "For example, you would adjust circle 1 until the area of circle 1 matches the area of rectangle 1. ", 'Col', black);
    RDCL_DrawText(s_middle_x, s_middle_y+250, "You then repeat this process for each circle. You can adjust any circle on the screen at any time. ", 'Col', black);
    RDCL_DrawText(s_middle_x, s_middle_y+300, "Press 'space' to continue the instructions", 'Col', black);
    Screen('Flip', screen_ptr); 
    WaitSecs(1.5);
    RDCL_GetResponse({{'space', 0}});

    
    Screen('FillRect', screen_ptr, white);
    RDCL_DrawText(s_middle_x, s_middle_y-300, "You adjust the circles by clicking and dragging the mouse.", 'Col', black);
    RDCL_DrawText(s_middle_x, s_middle_y-250, "When you’re done adjusting the circles, press the ‘submit’ button to continue to the next trial.", 'Col', black);
    RDCL_DrawText(s_middle_x, s_middle_y-200, "We’ll keep a running score of how close you are to the correct area and update you every so often.", 'Col', black);
    RDCL_DrawText(s_middle_x, s_middle_y-150, "Press ‘space’ to continue the instructions.", 'Col', black);
    Screen('Flip', screen_ptr); 
    RDCL_GetResponse({{'space', 0}});
    
    Screen('FillRect', screen_ptr, white);
    RDCL_DrawText(s_middle_x, s_middle_y-100, "We’re going to start out with three practice trials. These trials won’t count towards your performance. They are just here to get you familiar with the task.", 'Col', black);
    RDCL_DrawText(s_middle_x, s_middle_y-75, "Press 'space' to continue the instructions.", 'Col', black);
    Screen('Flip', screen_ptr); 
    WaitSecs(1.5);
    RDCL_GetResponse({{'space', 0}});
    
    Screen('FillRect', screen_ptr, white);
    RDCL_DrawText(s_middle_x, s_middle_y-100, "If you have any questions, now is a good time to ask the experimenter.", 'Col', black);
    RDCL_DrawText(s_middle_x, s_middle_y-75, "Press ‘space’ to start the practice trials.", 'Col', black);
    Screen('Flip', screen_ptr); 
    WaitSecs(1.5);
    RDCL_GetResponse({{'space', 0}});
    
    % START CIRCLE PRACTICE TRIALS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    for trial_index = 1:n_circle_prac
      % INIT variable keeping track of last time the mouse button was up
      last_mouse_up = [0 0];
      effect_tmp = 'practice';
      set_tmp = 'NA';
      diag_tmp = 999;
      distance_tmp = 999;
      block_num = 0;
      r1_tmp = sample_filler(min_stim,max_stim);
      r2_tmp = sample_filler(min_stim,max_stim);
      r3_tmp = sample_filler(min_stim,max_stim);
      w_all_tmp_unshuffled = [r1_tmp(1) r2_tmp(1) r3_tmp(1)];
      h_all_tmp_unshuffled = [r1_tmp(2) r2_tmp(2) r3_tmp(2)];
      % SHUFFLE RECTANGLE ORDER ON THIS TRIAL
      rects_order_tmp = RDCL_RandomizeTrials(3);

      % assigned shuffled rects to h1, h2, h3
      h1_tmp = round(h_all_tmp_unshuffled(rects_order_tmp(1)));
      h2_tmp = round(h_all_tmp_unshuffled(rects_order_tmp(2)));
      h3_tmp = round(h_all_tmp_unshuffled(rects_order_tmp(3)));

      % assigned shuffled rects to w1, w2, w3
      w1_tmp = round(w_all_tmp_unshuffled(rects_order_tmp(1)));
      w2_tmp = round(w_all_tmp_unshuffled(rects_order_tmp(2)));
      w3_tmp = round(w_all_tmp_unshuffled(rects_order_tmp(3)));

      % array of all heights SHUFFLED
      h_all_tmp_shuffled = [h1_tmp h2_tmp h3_tmp];

      % array of all widths SHUFFLED
      w_all_tmp_shuffled = [w1_tmp w2_tmp w3_tmp];
      
      % add vertical noise (important-same y noise for rect and coresponding circle)
      r1_jit = add_noise(vjit);
      r2_jit = add_noise(vjit);
      r3_jit = add_noise(vjit);
        
      % DETERMINE RECTANGLE POS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      % Rectangle 1 center x pos
      r1_x = rectangle_box_c(1)-(.5*w1_tmp)-(rect_dist_px)-(.5*w2_tmp);
 
      % Rectangle 2 center x pos
      r2_x = rectangle_box_c(1);
      % rectangle 3 center x pos
      r3_x = rectangle_box_c(1)+(.5*w2_tmp)+(rect_dist_px)+(.5*w3_tmp);
      
      % HORIZONTAL (Trueblood style) display %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      if strcmp(disp_cond,"horizontal")
        
        r1_y = rectangle_box_c(2)+r1_jit;
        r2_y = rectangle_box_c(2)+r2_jit;
        r3_y = rectangle_box_c(2)+r3_jit;
        

      % TRIANGLE (Spektor style) display    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      elseif strcmp(disp_cond, "triangle")
          r1_y = round(rectangle_box_c(2)+(.5*rect_dist_px)+.5*h1_tmp+r1_jit);
          r2_y = round(rectangle_box_c(2)-(.5*rect_dist_px)-.5*h2_tmp+r2_jit);
          r3_y = round(rectangle_box_c(2)+(.5*rect_dist_px)+.5*h3_tmp+r3_jit);           
      endif 


      % Center rectangles 
      r1_rect = CenterRectOnPoint([0 0 w1_tmp h1_tmp], r1_x, r1_y);
      r2_rect = CenterRectOnPoint([0 0 w2_tmp h2_tmp], r2_x, r2_y);
      r3_rect = CenterRectOnPoint([0 0 w3_tmp h3_tmp], r3_x, r3_y);

      % CIRCLE LOCATION %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%              
      % figure out where circles go
      circle2_square_x_center = circle_quadrant_center(1)

      circle1_square_x_center = circle2_square_x_center-circle_square_max_length*1-rect_dist_px;
      circle3_square_x_center = circle2_square_x_center+circle_square_max_length*1+rect_dist_px;      
      
      if strcmp(disp_cond,"horizontal")
        circle1_square_y_center = round((circle_quadrant(4)*.5)+r1_jit);
        circle2_square_y_center = round((circle_quadrant(4)*.5)+r2_jit);
        circle3_square_y_center = round((circle_quadrant(4)*.5)+r3_jit);
      elseif strcmp(disp_cond,"triangle")
        circle1_square_y_center = round( circle_quadrant_center(2)+(.5*rect_dist_px)+.5*circle_square_max_length+r1_jit);
        circle2_square_y_center = round( circle_quadrant_center(2)-(.5*rect_dist_px)-.5*circle_square_max_length+r2_jit);
        circle3_square_y_center = round( circle_quadrant_center(2)+(.5*rect_dist_px)+.5*circle_square_max_length+r3_jit);         
      endif
      
      circle1_square = [
        round(circle1_square_x_center-circle_square_max_length*.5)
        round(circle1_square_y_center-circle_square_max_length*.5)
        round(circle1_square_x_center+circle_square_max_length*.5)
        round(circle1_square_y_center+circle_square_max_length*.5)
      ];

      circle2_square = [
        round(circle2_square_x_center-circle_square_max_length*.5)
        round(circle2_square_y_center-circle_square_max_length*.5)
        round(circle2_square_x_center+circle_square_max_length*.5)
        round(circle2_square_y_center+circle_square_max_length*.5)
      ];

      circle3_square = [
        round(circle3_square_x_center-circle_square_max_length*.5)
        round(circle3_square_y_center-circle_square_max_length*.5)
        round(circle3_square_x_center+circle_square_max_length*.5)
        round(circle3_square_y_center+circle_square_max_length*.5)
      ];

      % TEXT LABEL LOCATIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      % RECRTANGLE LABELS
      rtxt_y = s_rect(4) - 30;
      
      % Rectangle 1 text center
      r1_txt_x = r1_x;

      r1_txt_y = rtxt_y;
      % Rectangle 2 text center
      r2_txt_x = r2_x;
      r2_txt_y = rtxt_y;

      % Rectangle 3 text center
      r3_txt_x = r3_x;
      r3_txt_y = rtxt_y;
      
      % CIRCLE LABELS
      ctxt_y = max([circle1_square(4) 
                    circle2_square(4)
                    circle3_square(4)])+round(.05*circle_quadrant_height);
      c1_txt_x = circle1_square_x_center;
      c2_txt_x = circle2_square_x_center;
      c3_txt_x = circle3_square_x_center;
      c1_txt_y = ctxt_y;
      c2_txt_y = ctxt_y;
      c3_txt_y = ctxt_y;        

      % get starting circle areas      
      circle1_area_tmp = min_circle_area;
      circle2_area_tmp = min_circle_area;
      circle3_area_tmp = min_circle_area;

      % get starting circle radius
      circle1_rad_tmp = rad_from_area(circle1_area_tmp);
      % "square" where we draw the circle
      circle1_square_tmp = [
        circle1_square_x_center-circle1_rad_tmp
        circle1_square_y_center-circle1_rad_tmp
        circle1_square_x_center+circle1_rad_tmp
        circle1_square_y_center+circle1_rad_tmp
      ];
      % get starting circle radius
      circle2_rad_tmp = rad_from_area(circle2_area_tmp);
      % "square" where we draw the circle
      circle2_square_tmp = [
        circle2_square_x_center-circle2_rad_tmp
        circle2_square_y_center-circle2_rad_tmp
        circle2_square_x_center+circle2_rad_tmp
        circle2_square_y_center+circle2_rad_tmp
      ];

      % get starting circle radius
      circle3_rad_tmp = rad_from_area(circle3_area_tmp);

      % "square" where we draw the circle
      circle3_square_tmp = [
        circle3_square_x_center-circle3_rad_tmp
        circle3_square_y_center-circle3_rad_tmp
        circle3_square_x_center+circle3_rad_tmp
        circle3_square_y_center+circle3_rad_tmp
      ];

      % INITIAL STIMULUS PRESENTATION %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      % NEED TO DO SEPARATE OF ADJUSTMENT LOOP FOR RT PURPOSES 
      % draw rectangles
      Screen('FillRect', screen_ptr, rect_fill, r1_rect);
      Screen('FillRect', screen_ptr, rect_fill, r2_rect);
      Screen('FillRect', screen_ptr, rect_fill, r3_rect);

      % DRAW CIRCLES
      Screen('FillOval', screen_ptr , circle_fill, circle1_square_tmp);
      Screen('FillOval', screen_ptr , circle_fill, circle2_square_tmp);
      Screen('FillOval', screen_ptr , circle_fill, circle3_square_tmp);

      % draw submit button
      % initially make gray
      Screen('FrameRect', screen_ptr, black, submit_rect, submit_rect_thick);
      RDCL_DrawText(submit_rect_c(1), submit_rect_c(2), "SUBMIT", 'Col', rect_fill);

      % draw labels
      RDCL_DrawText(r1_txt_x, r1_txt_y, "1", 'Col', black);
      RDCL_DrawText(r2_txt_x, r2_txt_y, "2", 'Col', black);
      RDCL_DrawText(r3_txt_x, r3_txt_y, "3", 'Col', black);

      RDCL_DrawText(c1_txt_x, c1_txt_y, "1", 'Col', black);
      RDCL_DrawText(c2_txt_x, c2_txt_y, "2", 'Col', black);
      RDCL_DrawText(c3_txt_x, c3_txt_y, "3", 'Col', black);

      % SHOW EVERYTHING 
      [trial_onset_time] = Screen('Flip', screen_ptr);
    
      % ADJUSTMENT PHASE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      % keep track of whether ppt is still adjusting
      adjusting=true;

      
      while adjusting
        
        % CHECK CURRENT MOUSE POS
        [click_x click_y buttons] = GetMouse(screen_ptr);

        % COMPUTE CURRENT RT
        rt_tmp = GetSecs()-trial_onset_time;
%        disp(buttons);
        % CHECK IF MOUSE BUTTON DOWN
        if buttons(1,1) 

          % BUTTON DOWN IN CIRCLE 1 SQUARE & LAST TIME UP IN CIRCLE 1 SQUARE
          if click_x >= circle1_square(1) && click_x < circle2_square(1) && click_y<=circle1_square(4) && last_mouse_up(1) >= circle1_square(1) && last_mouse_up(1) < circle2_square(1) 
            
            circle1_rad_start = [circle1_square_x_center circle1_square_y_center];
            circle1_rad_end = [click_x click_y];
            circle1_rad_tmp = compute_distance(circle1_rad_start, circle1_rad_end); 
            circle1_area_tmp=area_from_rad(circle1_rad_tmp);
            if circle1_area_tmp > max_circle_area
              circle1_rad_tmp = rad_from_area(max_circle_area);
              circle1_area_tmp = max_circle_area;
            elseif circle1_area_tmp < min_circle_area
              circle1_rad_tmp = min_circle_rad;
              circle1_area_tmp = min_circle_area;
            endif
          
            % "square" where we draw the circle
              circle1_square_tmp = [
                circle1_square_x_center-circle1_rad_tmp
                circle1_square_y_center-circle1_rad_tmp
                circle1_square_x_center+circle1_rad_tmp
                circle1_square_y_center+circle1_rad_tmp
            ];
          
          % BUTTON DOWN IN CIRCLE 2 SQUARE & LAST TIME UP IN CIRCLE 2 SQUARE 
          elseif  click_x >= circle2_square(1) && click_x < circle3_square(1) && click_y<=circle2_square(4) && last_mouse_up(1) >= circle2_square(1) && last_mouse_up(1) < circle3_square(1) 
            circle2_rad_start = [circle2_square_x_center circle2_square_y_center];
            circle2_rad_end = [click_x click_y];
            circle2_rad_tmp = compute_distance(circle2_rad_start, circle2_rad_end); 
            circle2_area_tmp=area_from_rad(circle2_rad_tmp);
            if circle2_area_tmp > max_circle_area
              circle2_rad_tmp = rad_from_area(max_circle_area);
              circle2_area_tmp = max_circle_area;
            elseif circle2_area_tmp < min_circle_area
              circle2_rad_tmp = min_circle_rad;
              circle2_area_tmp = min_circle_area;
            endif

            % "square" where we draw the circle
            circle2_square_tmp = [
              circle2_square_x_center-circle2_rad_tmp
              circle2_square_y_center-circle2_rad_tmp
              circle2_square_x_center+circle2_rad_tmp
              circle2_square_y_center+circle2_rad_tmp
          ];
                        
          % BUTTON DOWN IN CIRCLE 3 SQUARE & LAST TIME UP IN CIRCLE 3 SQUARE
          elseif click_x >= circle3_square(1) &&click_x < circle3_square(3) &&click_y<=circle3_square(4) &&last_mouse_up(1) >= circle3_square(1)

            circle3_rad_start = [circle3_square_x_center circle3_square_y_center];
            circle3_rad_end = [click_x click_y];
            circle3_rad_tmp = compute_distance(circle3_rad_start, circle3_rad_end); 
            circle3_area_tmp=area_from_rad(circle3_rad_tmp);
            if circle3_area_tmp > max_circle_area
                circle3_rad_tmp = rad_from_area(max_circle_area);
                circle3_area_tmp = max_circle_area;
            elseif circle3_area_tmp < min_circle_area
                circle3_rad_tmp = min_circle_rad;
                circle3_area_tmp = min_circle_area;
            endif
          
            % "square" where we draw the circle
            circle3_square_tmp = [
              circle3_square_x_center-circle3_rad_tmp
              circle3_square_y_center-circle3_rad_tmp
              circle3_square_x_center+circle3_rad_tmp
              circle3_square_y_center+circle3_rad_tmp
            ];
          
          % BUTTON DOWN IN SUBMIT BOX & LAST TIME UP IN SUBMIT BOX
          elseif click_x >= submit_rect(1) &&click_x < submit_rect(3) &&click_y < submit_rect(4) &&click_y >= submit_rect(2) &&rt_tmp >=submit_min &&last_mouse_up(1) >= submit_rect(1) &&last_mouse_up(1) < submit_rect(3) &&last_mouse_up(2) < submit_rect(4) &&last_mouse_up(2) >= submit_rect(2)
            
            r1_diff = (h1_tmp*w1_tmp)-circle1_area_tmp;
            r2_diff = (h2_tmp*w2_tmp)-circle2_area_tmp;
            r3_diff = (h3_tmp*w3_tmp)-circle3_area_tmp;
            circle_error = [circle_error(1:trial_index-1) r1_diff r2_diff r3_diff];
            fprintf(fp_circle, 
            '%s, %d, %s, %s, %s, %d, %d, %d, %d, %d, %d, %d, %d, %d, %d, %d, %d, %d, %d, %d, %d, %d\n',
            sub_n, computer_num, disp_cond, effect_tmp, set_tmp, distance_tmp, diag_tmp, block_num, trial_index, 
            h1_tmp, w1_tmp, h2_tmp, w2_tmp, h3_tmp, w3_tmp, circle1_area_tmp, circle1_rad_tmp, circle2_area_tmp, circle2_rad_tmp, circle3_area_tmp, circle3_rad_tmp, rt_tmp);
            fflush(fp_circle);
            adjusting=false;
          endif

          % MOUSE UP  
          elseif ~any(buttons)
            last_mouse_up = [click_x click_y];
      endif  
      
    % clear screen
    Screen('FillRect',screen_ptr,white);
    % draw rectangles
    Screen('FillRect', screen_ptr, rect_fill, r1_rect);
    Screen('FillRect', screen_ptr, rect_fill, r2_rect);
    Screen('FillRect', screen_ptr, rect_fill, r3_rect);
    % DRAW CIRCLES
    Screen('FillOval', screen_ptr , circle_fill, circle1_square_tmp);
    Screen('FillOval', screen_ptr , circle_fill, circle2_square_tmp);
    Screen('FillOval', screen_ptr , circle_fill, circle3_square_tmp);

    % draw submit button
    Screen('FrameRect', screen_ptr, black, submit_rect, submit_rect_thick);
    if(rt_tmp>=submit_min)
      RDCL_DrawText(submit_rect_c(1), submit_rect_c(2), "SUBMIT", 'Col', black);
    else
      RDCL_DrawText(submit_rect_c(1), submit_rect_c(2), "SUBMIT", 'Col', rect_fill);
    endif 
    
    % draw labels
    RDCL_DrawText(r1_txt_x, r1_txt_y, "1", 'Col', black);
    RDCL_DrawText(r2_txt_x, r2_txt_y, "2", 'Col', black);
    RDCL_DrawText(r3_txt_x, r3_txt_y, "3", 'Col', black);
    
    RDCL_DrawText(c1_txt_x, c1_txt_y, "1", 'Col', black);
    RDCL_DrawText(c2_txt_x, c2_txt_y, "2", 'Col', black);
    RDCL_DrawText(c3_txt_x, c3_txt_y, "3", 'Col', black);
    % SHOW EVERYTHING
    Screen('Flip', screen_ptr);           
    WaitSecs(lag_time);
  endwhile
  WaitSecs(.5);
endfor 

Screen('FillRect', screen_ptr, white);
RDCL_DrawText(s_middle_x, s_middle_y-100, "Thank you! You just completed the practice trials.", 'Col', black);
RDCL_DrawText(s_middle_x, s_middle_y-75, "We are going to start the experiment shortly.", 'Col', black);
RDCL_DrawText(s_middle_x, s_middle_y-50, "However, before we do, we will have three 'calibration' trials.", 'Col', black);
RDCL_DrawText(s_middle_x, s_middle_y-25, "Press 'space' to continue the instructions.", 'Col', black);
Screen('Flip', screen_ptr);
WaitSecs(0.5);
RDCL_GetResponse({{'space', 0}});

Screen('FillRect', screen_ptr, white);
RDCL_DrawText(s_middle_x, s_middle_y-100, "We know this task is hard, so these calibration trials will help you do the task more easily.", 'Col', black);
RDCL_DrawText(s_middle_x, s_middle_y-75, "On the calibration trials, we will show you what the correct circle should look like by drawing a red circle around your submitted circle, after you hit the submit button.", 'Col', black);
RDCL_DrawText(s_middle_x, s_middle_y-25, "Press 'space' to continue the instructions.", 'Col', black);
Screen('Flip', screen_ptr); 
WaitSecs(1.5);
RDCL_GetResponse({{'space', 0}});

Screen('FillRect', screen_ptr, white);
RDCL_DrawText(s_middle_x, s_middle_y-100, "We will include these trials every so often in the experiment too, to help keep you on track.", 'Col', black);
RDCL_DrawText(s_middle_x, s_middle_y-75, "Press 'space' to continue to the calibration trials.", 'Col', black);
Screen('Flip', screen_ptr); 
WaitSecs(.5);
RDCL_GetResponse({{'space', 0}});

  % START CIRCLE TASK %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  % LOOP THROUGH BLOCKS
  for block_num=1:n_circle_blocks
    % figure out current error
    if block_num>1
      error_avg = mean(circle_error);
      circle_error = [];
      if(error_avg>0)
        error_text="On average you made the circles smaller than the true rectangle area. Try to adjust your performance accordingly!";
      else 
        error_text="On average you made the circles larger than the true rectangle area. Try to adjust your performance accordingly!";
      endif 
      Screen('FillRect', screen_ptr, white)
      RDCL_DrawText(s_middle_x, s_middle_y-25, error_text, 'Col', black);
      RDCL_DrawText(s_middle_x, s_middle_y-75, "Press 'space' to continue.", 'Col', black);
      Screen('Flip', screen_ptr); 
      RDCL_GetResponse({{'space', 0}});
    endif
    Screen('FillRect', screen_ptr, white)
    RDCL_DrawText(s_middle_x, s_middle_y-25, "Time for some calibration trials!", 'Col', black);
    Screen('Flip', screen_ptr); 
    WaitSecs(1);
    Screen('FillRect', screen_ptr, white)
    Screen('Flip', screen_ptr); 
    for trial_index = 1:n_calib
          effect_tmp = 'calibration';
          set_tmp = 'NA';
          diag_tmp = 999;
          distance_tmp = 999;
          r1_tmp = sample_filler(min_stim,max_stim);
          r2_tmp = sample_filler(min_stim,max_stim);
          r3_tmp = sample_filler(min_stim,max_stim);
          w_all_tmp_unshuffled = [r1_tmp(1) r2_tmp(1) r3_tmp(1)];
          h_all_tmp_unshuffled = [r1_tmp(2) r2_tmp(2) r3_tmp(2)];
          % SHUFFLE RECTANGLE ORDER ON THIS TRIAL
          rects_order_tmp = RDCL_RandomizeTrials(3);

          % assigned shuffled rects to h1, h2, h3
          h1_tmp = round(h_all_tmp_unshuffled(rects_order_tmp(1)));
          h2_tmp = round(h_all_tmp_unshuffled(rects_order_tmp(2)));
          h3_tmp = round(h_all_tmp_unshuffled(rects_order_tmp(3)));

          % assigned shuffled rects to w1, w2, w3
          w1_tmp = round(w_all_tmp_unshuffled(rects_order_tmp(1)));
          w2_tmp = round(w_all_tmp_unshuffled(rects_order_tmp(2)));
          w3_tmp = round(w_all_tmp_unshuffled(rects_order_tmp(3)));

          % array of all heights SHUFFLED
          h_all_tmp_shuffled = [h1_tmp h2_tmp h3_tmp];

          % array of all widths SHUFFLED
          w_all_tmp_shuffled = [w1_tmp w2_tmp w3_tmp];
          
          % add vertical noise (important-same y noise for rect and coresponding circle)
          r1_jit = add_noise(vjit);
          r2_jit = add_noise(vjit);
          r3_jit = add_noise(vjit);
            
          % DETERMINE RECTANGLE POS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
          % Rectangle 1 center x pos
          r1_x = rectangle_box_c(1)-(.5*w1_tmp)-(rect_dist_px)-(.5*w2_tmp);
          
          % Rectangle 2 center x pos
          r2_x = rectangle_box_c(1);
          % rectangle 3 center x pos
          r3_x = rectangle_box_c(1)+(.5*w2_tmp)+(rect_dist_px)+(.5*w3_tmp);
          
          % HORIZONTAL (Trueblood style) display %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
          if strcmp(disp_cond,"horizontal")
            
            r1_y = rectangle_box_c(2)+r1_jit;
            r2_y = rectangle_box_c(2)+r2_jit;
            r3_y = rectangle_box_c(2)+r3_jit;
            

          % TRIANGLE (Spektor style) display    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
          elseif strcmp(disp_cond, "triangle")
              r1_y = round(rectangle_box_c(2)+(.5*rect_dist_px)+.5*h1_tmp+r1_jit);
              r2_y = round(rectangle_box_c(2)-(.5*rect_dist_px)-.5*h2_tmp+r2_jit);
              r3_y = round(rectangle_box_c(2)+(.5*rect_dist_px)+.5*h3_tmp+r3_jit);           
          endif 


          % Center rectangles 
          r1_rect = CenterRectOnPoint([0 0 w1_tmp h1_tmp], r1_x, r1_y);
          r2_rect = CenterRectOnPoint([0 0 w2_tmp h2_tmp], r2_x, r2_y);
          r3_rect = CenterRectOnPoint([0 0 w3_tmp h3_tmp], r3_x, r3_y);

          % CIRCLE LOCATION %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%              
          % figure out where circles go
          circle2_square_x_center = circle_quadrant_center(1)

          circle1_square_x_center = circle2_square_x_center-circle_square_max_length*1-rect_dist_px;
          circle3_square_x_center = circle2_square_x_center+circle_square_max_length*1+rect_dist_px;      
          
          if strcmp(disp_cond,"horizontal")
            circle1_square_y_center = round((circle_quadrant(4)*.5)+r1_jit);
            circle2_square_y_center = round((circle_quadrant(4)*.5)+r2_jit);
            circle3_square_y_center = round((circle_quadrant(4)*.5)+r3_jit);
          elseif strcmp(disp_cond,"triangle")
            circle1_square_y_center = round( circle_quadrant_center(2)+(.5*rect_dist_px)+.5*circle_square_max_length+r1_jit);
            circle2_square_y_center = round( circle_quadrant_center(2)-(.5*rect_dist_px)-.5*circle_square_max_length+r2_jit);
            circle3_square_y_center = round( circle_quadrant_center(2)+(.5*rect_dist_px)+.5*circle_square_max_length+r3_jit);         
          endif
          
          circle1_square = [
            round(circle1_square_x_center-circle_square_max_length*.5)
            round(circle1_square_y_center-circle_square_max_length*.5)
            round(circle1_square_x_center+circle_square_max_length*.5)
            round(circle1_square_y_center+circle_square_max_length*.5)
          ];

          circle2_square = [
            round(circle2_square_x_center-circle_square_max_length*.5)
            round(circle2_square_y_center-circle_square_max_length*.5)
            round(circle2_square_x_center+circle_square_max_length*.5)
            round(circle2_square_y_center+circle_square_max_length*.5)
          ];

          circle3_square = [
            round(circle3_square_x_center-circle_square_max_length*.5)
            round(circle3_square_y_center-circle_square_max_length*.5)
            round(circle3_square_x_center+circle_square_max_length*.5)
            round(circle3_square_y_center+circle_square_max_length*.5)
          ];

          % TEXT LABEL LOCATIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
          % RECRTANGLE LABELS
          rtxt_y = rectangle_box(4)-30;
          
          % Rectangle 1 text center
          r1_txt_x = r1_x;

          r1_txt_y = rtxt_y;
          % Rectangle 2 text center
          r2_txt_x = r2_x;
          r2_txt_y = rtxt_y;

          % Rectangle 3 text center
          r3_txt_x = r3_x;
          r3_txt_y = rtxt_y;
          
          % CIRCLE LABELS
          ctxt_y = max([circle1_square(4) 
                        circle2_square(4)
                        circle3_square(4)])+round(.05*circle_quadrant_height);
          c1_txt_x = circle1_square_x_center;
          c2_txt_x = circle2_square_x_center;
          c3_txt_x = circle3_square_x_center;
          c1_txt_y = ctxt_y;
          c2_txt_y = ctxt_y;
          c3_txt_y = ctxt_y;        

          % get starting circle areas      
          circle1_area_tmp = min_circle_area;
          circle2_area_tmp = min_circle_area;
          circle3_area_tmp = min_circle_area;

          % get starting circle radius
          circle1_rad_tmp = rad_from_area(circle1_area_tmp);
          % "square" where we draw the circle
          circle1_square_tmp = [
            circle1_square_x_center-circle1_rad_tmp
            circle1_square_y_center-circle1_rad_tmp
            circle1_square_x_center+circle1_rad_tmp
            circle1_square_y_center+circle1_rad_tmp
          ];
          % get starting circle radius
          circle2_rad_tmp = rad_from_area(circle2_area_tmp);
          % "square" where we draw the circle
          circle2_square_tmp = [
            circle2_square_x_center-circle2_rad_tmp
            circle2_square_y_center-circle2_rad_tmp
            circle2_square_x_center+circle2_rad_tmp
            circle2_square_y_center+circle2_rad_tmp
          ];

          % get starting circle radius
          circle3_rad_tmp = rad_from_area(circle3_area_tmp);

          % "square" where we draw the circle
          circle3_square_tmp = [
            circle3_square_x_center-circle3_rad_tmp
            circle3_square_y_center-circle3_rad_tmp
            circle3_square_x_center+circle3_rad_tmp
            circle3_square_y_center+circle3_rad_tmp
          ];

          % INITIAL STIMULUS PRESENTATION %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
          % NEED TO DO SEPARATE OF ADJUSTMENT LOOP FOR RT PURPOSES 
          % draw rectangles
          Screen('FillRect', screen_ptr, rect_fill, r1_rect);
          Screen('FillRect', screen_ptr, rect_fill, r2_rect);
          Screen('FillRect', screen_ptr, rect_fill, r3_rect);

          % DRAW CIRCLES
          Screen('FillOval', screen_ptr , circle_fill, circle1_square_tmp);
          Screen('FillOval', screen_ptr , circle_fill, circle2_square_tmp);
          Screen('FillOval', screen_ptr , circle_fill, circle3_square_tmp);

          % draw submit button
          % initially make gray
          Screen('FrameRect', screen_ptr, black, submit_rect, submit_rect_thick);
          RDCL_DrawText(submit_rect_c(1), submit_rect_c(2), "SUBMIT", 'Col', rect_fill);

          % draw labels
          RDCL_DrawText(r1_txt_x, r1_txt_y, "1", 'Col', black);
          RDCL_DrawText(r2_txt_x, r2_txt_y, "2", 'Col', black);
          RDCL_DrawText(r3_txt_x, r3_txt_y, "3", 'Col', black);

          RDCL_DrawText(c1_txt_x, c1_txt_y, "1", 'Col', black);
          RDCL_DrawText(c2_txt_x, c2_txt_y, "2", 'Col', black);
          RDCL_DrawText(c3_txt_x, c3_txt_y, "3", 'Col', black);

          % SHOW EVERYTHING 
          [trial_onset_time] = Screen('Flip', screen_ptr);
        
          % ADJUSTMENT PHASE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
          % keep track of whether ppt is still adjusting
          adjusting=true;

          % INIT variable keeping track of last time the mouse button was up
          last_mouse_up = [0 0];
          while adjusting
            
            % CHECK CURRENT MOUSE POS
            [click_x click_y buttons] = GetMouse(screen_ptr);

            % COMPUTE CURRENT RT
            rt_tmp = GetSecs()-trial_onset_time;
            
            % CHECK IF MOUSE BUTTON DOWN
            if buttons(1,1) 

              % BUTTON DOWN IN CIRCLE 1 SQUARE & LAST TIME UP IN CIRCLE 1 SQUARE
              if click_x >= circle1_square(1) && click_x < circle2_square(1) && click_y<=circle1_square(4) && last_mouse_up(1) >= circle1_square(1) && last_mouse_up(1) < circle2_square(1) 
                
                circle1_rad_start = [circle1_square_x_center circle1_square_y_center];
                circle1_rad_end = [click_x click_y];
                circle1_rad_tmp = compute_distance(circle1_rad_start, circle1_rad_end); 
                circle1_area_tmp=area_from_rad(circle1_rad_tmp);
                if circle1_area_tmp > max_circle_area
                  circle1_rad_tmp = rad_from_area(max_circle_area);
                  circle1_area_tmp = max_circle_area;
                elseif circle1_area_tmp < min_circle_area
                  circle1_rad_tmp = min_circle_rad;
                  circle1_area_tmp = min_circle_area;
                endif
              
                % "square" where we draw the circle
                  circle1_square_tmp = [
                    circle1_square_x_center-circle1_rad_tmp
                    circle1_square_y_center-circle1_rad_tmp
                    circle1_square_x_center+circle1_rad_tmp
                    circle1_square_y_center+circle1_rad_tmp
                ];
              
              % BUTTON DOWN IN CIRCLE 2 SQUARE & LAST TIME UP IN CIRCLE 2 SQUARE 
              elseif  click_x >= circle2_square(1) && click_x < circle3_square(1) && click_y<=circle2_square(4) && last_mouse_up(1) >= circle2_square(1) && last_mouse_up(1) < circle3_square(1) 
                circle2_rad_start = [circle2_square_x_center circle2_square_y_center];
                circle2_rad_end = [click_x click_y];
                circle2_rad_tmp = compute_distance(circle2_rad_start, circle2_rad_end); 
                circle2_area_tmp=area_from_rad(circle2_rad_tmp);
                if circle2_area_tmp > max_circle_area
                  circle2_rad_tmp = rad_from_area(max_circle_area);
                  circle2_area_tmp = max_circle_area;
                elseif circle2_area_tmp < min_circle_area
                  circle2_rad_tmp = min_circle_rad;
                  circle2_area_tmp = min_circle_area;
                endif

                % "square" where we draw the circle
                circle2_square_tmp = [
                  circle2_square_x_center-circle2_rad_tmp
                  circle2_square_y_center-circle2_rad_tmp
                  circle2_square_x_center+circle2_rad_tmp
                  circle2_square_y_center+circle2_rad_tmp
              ];
                            
              % BUTTON DOWN IN CIRCLE 3 SQUARE & LAST TIME UP IN CIRCLE 3 SQUARE
              elseif click_x >= circle3_square(1) &&click_x < circle3_square(3) &&click_y<=circle3_square(4) &&last_mouse_up(1) >= circle3_square(1) &&last_mouse_up(1) < circle3_square(3) 

                circle3_rad_start = [circle3_square_x_center circle3_square_y_center];
                circle3_rad_end = [click_x click_y];
                circle3_rad_tmp = compute_distance(circle3_rad_start, circle3_rad_end); 
                circle3_area_tmp=area_from_rad(circle3_rad_tmp);
                if circle3_area_tmp > max_circle_area
                    circle3_rad_tmp = rad_from_area(max_circle_area);
                    circle3_area_tmp = max_circle_area;
                elseif circle3_area_tmp < min_circle_area
                    circle3_rad_tmp = min_circle_rad;
                    circle3_area_tmp = min_circle_area;
                endif
              
                % "square" where we draw the circle
                circle3_square_tmp = [
                  circle3_square_x_center-circle3_rad_tmp
                  circle3_square_y_center-circle3_rad_tmp
                  circle3_square_x_center+circle3_rad_tmp
                  circle3_square_y_center+circle3_rad_tmp
                ];
              
              % BUTTON DOWN IN SUBMIT BOX & LAST TIME UP IN SUBMIT BOX
              elseif click_x >= submit_rect(1) &&click_x < submit_rect(3) &&click_y < submit_rect(4) &&click_y >= submit_rect(2) &&rt_tmp >=submit_min &&last_mouse_up(1) >= submit_rect(1) &&last_mouse_up(1) < submit_rect(3) &&last_mouse_up(2) < submit_rect(4) &&last_mouse_up(2) >= submit_rect(2)
                
              
                fprintf(fp_circle, 
                '%s, %d, %s, %s, %s, %d, %d, %d, %d, %d, %d, %d, %d, %d, %d, %d, %d, %d, %d, %d, %d, %d\n',
                sub_n, computer_num, disp_cond, effect_tmp, set_tmp, distance_tmp, diag_tmp, block_num, trial_index, 
                h1_tmp, w1_tmp, h2_tmp, w2_tmp, h3_tmp, w3_tmp, circle1_area_tmp, circle1_rad_tmp, circle2_area_tmp, circle2_rad_tmp, circle3_area_tmp, circle3_rad_tmp, rt_tmp);
                fflush(fp_circle);
                
                Screen('FillRect', screen_ptr, white);
                Screen('Flip', screen_ptr); 
                WaitSecs(.1);
                
                Screen('FillRect', screen_ptr, rect_fill, r1_rect);
                Screen('FillRect', screen_ptr, rect_fill, r2_rect);
                Screen('FillRect', screen_ptr, rect_fill, r3_rect);

                % DRAW CIRCLES
                Screen('FillOval', screen_ptr , circle_fill, circle1_square_tmp);
                Screen('FillOval', screen_ptr , circle_fill, circle2_square_tmp);
                Screen('FillOval', screen_ptr , circle_fill, circle3_square_tmp);

                % draw submit button
                % initially make gray
                Screen('FrameRect', screen_ptr, black, submit_rect, submit_rect_thick);
                RDCL_DrawText(submit_rect_c(1), submit_rect_c(2), "SUBMIT", 'Col', rect_fill);
                
                
                  adjusting=false;
                  Screen('FillRect', screen_ptr, white);
                  Screen('Flip', screen_ptr); 
                  WaitSecs(lag_time);
        endif

              % MOUSE UP  
                        % MOUSE UP  
          elseif ~any(buttons)
            last_mouse_up = [click_x click_y];

          endif  
          
        % clear screen
        Screen('FillRect',screen_ptr,white);
        % draw rectangles
        Screen('FillRect', screen_ptr, rect_fill, r1_rect);
        Screen('FillRect', screen_ptr, rect_fill, r2_rect);
        Screen('FillRect', screen_ptr, rect_fill, r3_rect);
        % DRAW CIRCLES
        Screen('FillOval', screen_ptr , circle_fill, circle1_square_tmp);
        Screen('FillOval', screen_ptr , circle_fill, circle2_square_tmp);
        Screen('FillOval', screen_ptr , circle_fill, circle3_square_tmp);

        % draw submit button
        Screen('FrameRect', screen_ptr, black, submit_rect, submit_rect_thick);
        if(rt_tmp>=submit_min)
          RDCL_DrawText(submit_rect_c(1), submit_rect_c(2), "SUBMIT", 'Col', black);
        else
          RDCL_DrawText(submit_rect_c(1), submit_rect_c(2), "SUBMIT", 'Col', rect_fill);
        endif 
        % draw labels
        RDCL_DrawText(r1_txt_x, r1_txt_y, "1", 'Col', black);
        RDCL_DrawText(r2_txt_x, r2_txt_y, "2", 'Col', black);
        RDCL_DrawText(r3_txt_x, r3_txt_y, "3", 'Col', black);
        
        RDCL_DrawText(c1_txt_x, c1_txt_y, "1", 'Col', black);
        RDCL_DrawText(c2_txt_x, c2_txt_y, "2", 'Col', black);
        RDCL_DrawText(c3_txt_x, c3_txt_y, "3", 'Col', black);
        % SHOW EVERYTHING
        Screen('Flip', screen_ptr);
        WaitSecs(lag_time);
      endwhile
      WaitSecs(.001);
      corr_circle1_rad_tmp = rad_from_area(h1_tmp*w1_tmp);
                  % "square" where we draw the circle
                  corr_circle1_sq_tmp = [
                  circle1_square_x_center-corr_circle1_rad_tmp
                  circle1_square_y_center-corr_circle1_rad_tmp
                  circle1_square_x_center+corr_circle1_rad_tmp
                  circle1_square_y_center+corr_circle1_rad_tmp];
                      
                  corr_circle2_rad_tmp = rad_from_area(h2_tmp*w2_tmp);
                  % "square" where we draw the circle
                  corr_circle3_rad_tmp = rad_from_area(h3_tmp*w3_tmp);

                  corr_circle2_sq_tmp = [
                  circle2_square_x_center-corr_circle2_rad_tmp
                  circle2_square_y_center-corr_circle2_rad_tmp
                  circle2_square_x_center+corr_circle2_rad_tmp
                  circle2_square_y_center+corr_circle2_rad_tmp];
                  corr_circle2_rad_tmp = rad_from_area(h2_tmp*w2_tmp);
                  % "square" where we draw the circle
                  corr_circle3_sq_tmp = [
                  circle3_square_x_center-corr_circle3_rad_tmp
                  circle3_square_y_center-corr_circle3_rad_tmp
                  circle3_square_x_center+corr_circle3_rad_tmp
                  circle3_square_y_center+corr_circle3_rad_tmp];
                  % DRAW CIRCLE
                  % DRAW CIRCLES
                  Screen('FillOval', screen_ptr , circle_fill, circle1_square_tmp);
                  Screen('FillOval', screen_ptr , circle_fill, circle2_square_tmp);
                  Screen('FillOval', screen_ptr , circle_fill, circle3_square_tmp);
                    % DRAW CIRCLES
                  Screen('FrameOval', screen_ptr , red, corr_circle1_sq_tmp);
                  Screen('FrameOval', screen_ptr , red, corr_circle2_sq_tmp);
                  Screen('FrameOval', screen_ptr , red, corr_circle3_sq_tmp);
                  Screen('FillRect', screen_ptr, rect_fill, r1_rect);
                  Screen('FillRect', screen_ptr, rect_fill, r2_rect);
                  Screen('FillRect', screen_ptr, rect_fill, r3_rect);
                  % draw labels
                  RDCL_DrawText(r1_txt_x, r1_txt_y, "1", 'Col', black);
                  RDCL_DrawText(r2_txt_x, r2_txt_y, "2", 'Col', black);
                  RDCL_DrawText(r3_txt_x, r3_txt_y, "3", 'Col', black);
        
                  RDCL_DrawText(c1_txt_x, c1_txt_y, "1", 'Col', black);
                  RDCL_DrawText(c2_txt_x, c2_txt_y, "2", 'Col', black);
                  RDCL_DrawText(c3_txt_x, c3_txt_y, "3", 'Col', black);
                  Screen('Flip', screen_ptr); 
                  WaitSecs(3);
                  Screen('FillRect', screen_ptr, white);
                  Screen('Flip', screen_ptr); 
                  WaitSecs(1.6);
    endfor 
    Screen('FillRect', screen_ptr, white);
    RDCL_DrawText(s_middle_x, s_middle_y, "The regular experimental trials will resume.", 'Col', black);
    Screen('Flip', screen_ptr); 
    WaitSecs(3);
    
    % Randomize trial order for this block     
    trial_order = RDCL_RandomizeTrials(n_circle_trials)

      % LOOP THROUGH TRIALS
      for trial_index=1:n_circle_trials

        % Clear screen to start trial
        Screen('FillRect', screen_ptr, white);
        Screen('Flip', screen_ptr); 
        WaitSecs(.1);

        % CURRENT TRIAL PARAMS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        % Get shuffled trial number
        trial_num = trial_order(trial_index);

        % CHOICE SET
        set_tmp = sets{trial_num};

        % DISATANCE
        distance_tmp = distance(trial_num);

        % EFFECT (att/catch/filler)
        effect_tmp = effect{trial_num};

        % DIAGONAL
        diag_tmp = diag(trial_num);

        % GET RECT HEIGHT & WIDTH
        if strcmp(effect_tmp,"catch") 
          r1_tmp = sample_diag(d1_r, d1_int);
          r2_tmp = sample_diag(d1_r, d1_int);
          r3_tmp = sample_diag(d3_r, d3_int);
          w_all_tmp_unshuffled = [r1_tmp(1) r2_tmp(1) r3_tmp(1)];
          h_all_tmp_unshuffled = [r1_tmp(2) r2_tmp(2) r3_tmp(2)];
        elseif strcmp(effect_tmp,"filler")
          r1_tmp = sample_filler(min_stim,max_stim);
          r2_tmp = sample_filler(min_stim,max_stim);
          r3_tmp = sample_filler(min_stim,max_stim);
          w_all_tmp_unshuffled = [r1_tmp(1) r2_tmp(1) r3_tmp(1)];
          h_all_tmp_unshuffled = [r1_tmp(2) r2_tmp(2) r3_tmp(2)];
        elseif strcmp(effect_tmp,"attraction")
          h_all_tmp_unshuffled = [h1(trial_num) h2(trial_num) h3(trial_num)];
          w_all_tmp_unshuffled = [w1(trial_num) w2(trial_num) w3(trial_num)];
        endif

        % SHUFFLE RECTANGLE ORDER ON THIS TRIAL
        rects_order_tmp = RDCL_RandomizeTrials(3);

        % assigned shuffled rects to h1, h2, h3
        h1_tmp = round(h_all_tmp_unshuffled(rects_order_tmp(1)));
        h2_tmp = round(h_all_tmp_unshuffled(rects_order_tmp(2)));
        h3_tmp = round(h_all_tmp_unshuffled(rects_order_tmp(3)));

        % assigned shuffled rects to w1, w2, w3
        w1_tmp = round(w_all_tmp_unshuffled(rects_order_tmp(1)));
        w2_tmp = round(w_all_tmp_unshuffled(rects_order_tmp(2)));
        w3_tmp = round(w_all_tmp_unshuffled(rects_order_tmp(3)));

        % array of all heights SHUFFLED
        h_all_tmp_shuffled = [h1_tmp h2_tmp h3_tmp];

        % array of all widths SHUFFLED
        w_all_tmp_shuffled = [w1_tmp w2_tmp w3_tmp];
        
        % add vertical noise (important-same y noise for rect and coresponding circle)
        r1_jit = add_noise(vjit);
        r2_jit = add_noise(vjit);
        r3_jit = add_noise(vjit);
          
        % DETERMINE RECTANGLE POS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        % Rectangle 1 center x pos
        r1_x = rectangle_box_c(1)-(.5*w1_tmp)-(rect_dist_px)-(.5*w2_tmp);
        
        % Rectangle 2 center x pos
        r2_x = rectangle_box_c(1);
        % rectangle 3 center x pos
        r3_x = rectangle_box_c(1)+(.5*w2_tmp)+(rect_dist_px)+(.5*w3_tmp);
        
        % HORIZONTAL (Trueblood style) display %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        if strcmp(disp_cond,"horizontal")          
          r1_y = rectangle_box_c(2)+r1_jit;
          r2_y = rectangle_box_c(2)+r2_jit;
          r3_y = rectangle_box_c(2)+r3_jit;     
        % TRIANGLE (Spektor style) display    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        elseif strcmp(disp_cond, "triangle")
            r1_y = round(rectangle_box_c(2)+(.5*rect_dist_px)+.5*h1_tmp+r1_jit);
            r2_y = round(rectangle_box_c(2)-(.5*rect_dist_px)-.5*h2_tmp+r2_jit);
            r3_y = round(rectangle_box_c(2)+(.5*rect_dist_px)+.5*h3_tmp+r3_jit);           
        endif 

  
        % Center rectangles 
        r1_rect = CenterRectOnPoint([0 0 w1_tmp h1_tmp], r1_x, r1_y);
        r2_rect = CenterRectOnPoint([0 0 w2_tmp h2_tmp], r2_x, r2_y);
        r3_rect = CenterRectOnPoint([0 0 w3_tmp h3_tmp], r3_x, r3_y);

        % CIRCLE LOCATION %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%              
        % figure out where circles go
        circle2_square_x_center = circle_quadrant_center(1)

        circle1_square_x_center = circle2_square_x_center-circle_square_max_length*1-rect_dist_px;
        circle3_square_x_center = circle2_square_x_center+circle_square_max_length*1+rect_dist_px;      
        
        if strcmp(disp_cond,"horizontal")
          circle1_square_y_center = round((circle_quadrant(4)*.5)+r1_jit);
          circle2_square_y_center = round((circle_quadrant(4)*.5)+r2_jit);
          circle3_square_y_center = round((circle_quadrant(4)*.5)+r3_jit);
        elseif strcmp(disp_cond,"triangle")
          circle1_square_y_center = round( circle_quadrant_center(2)+(.5*rect_dist_px)+.5*circle_square_max_length+r1_jit);
          circle2_square_y_center = round( circle_quadrant_center(2)-(.5*rect_dist_px)-.5*circle_square_max_length+r2_jit);
          circle3_square_y_center = round( circle_quadrant_center(2)+(.5*rect_dist_px)+.5*circle_square_max_length+r3_jit);         
        endif
        
        circle1_square = [
          round(circle1_square_x_center-circle_square_max_length*.5)
          round(circle1_square_y_center-circle_square_max_length*.5)
          round(circle1_square_x_center+circle_square_max_length*.5)
          round(circle1_square_y_center+circle_square_max_length*.5)
        ];

        circle2_square = [
          round(circle2_square_x_center-circle_square_max_length*.5)
          round(circle2_square_y_center-circle_square_max_length*.5)
          round(circle2_square_x_center+circle_square_max_length*.5)
          round(circle2_square_y_center+circle_square_max_length*.5)
        ];

        circle3_square = [
          round(circle3_square_x_center-circle_square_max_length*.5)
          round(circle3_square_y_center-circle_square_max_length*.5)
          round(circle3_square_x_center+circle_square_max_length*.5)
          round(circle3_square_y_center+circle_square_max_length*.5)
        ];

        % TEXT LABEL LOCATIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        % RECRTANGLE LABELS
        rtxt_y = s_rect(4)-30;
        % Rectangle 1 text center
        r1_txt_x = r1_x;
  
        r1_txt_y = rtxt_y;
        % Rectangle 2 text center
        r2_txt_x = r2_x;
        r2_txt_y = rtxt_y;

        % Rectangle 3 text center
        r3_txt_x = r3_x;
        r3_txt_y = rtxt_y;
        
        % CIRCLE LABELS
        ctxt_y = max([circle1_square(4) 
                      circle2_square(4)
                      circle3_square(4)])+round(.05*circle_quadrant_height);
        c1_txt_x = circle1_square_x_center;
        c2_txt_x = circle2_square_x_center;
        c3_txt_x = circle3_square_x_center;
        c1_txt_y = ctxt_y;
        c2_txt_y = ctxt_y;
        c3_txt_y = ctxt_y;        

        % get starting circle areas      
        circle1_area_tmp = min_circle_area;
        circle2_area_tmp = min_circle_area;
        circle3_area_tmp = min_circle_area;

        % get starting circle radius
        circle1_rad_tmp = rad_from_area(circle1_area_tmp);
        % "square" where we draw the circle
        circle1_square_tmp = [
          circle1_square_x_center-circle1_rad_tmp
          circle1_square_y_center-circle1_rad_tmp
          circle1_square_x_center+circle1_rad_tmp
          circle1_square_y_center+circle1_rad_tmp
        ];
        % get starting circle radius
        circle2_rad_tmp = rad_from_area(circle2_area_tmp);
        % "square" where we draw the circle
        circle2_square_tmp = [
          circle2_square_x_center-circle2_rad_tmp
          circle2_square_y_center-circle2_rad_tmp
          circle2_square_x_center+circle2_rad_tmp
          circle2_square_y_center+circle2_rad_tmp
        ];

        % get starting circle radius
        circle3_rad_tmp = rad_from_area(circle3_area_tmp);

        % "square" where we draw the circle
        circle3_square_tmp = [
          circle3_square_x_center-circle3_rad_tmp
          circle3_square_y_center-circle3_rad_tmp
          circle3_square_x_center+circle3_rad_tmp
          circle3_square_y_center+circle3_rad_tmp
        ];

        % INITIAL STIMULUS PRESENTATION %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        % NEED TO DO SEPARATE OF ADJUSTMENT LOOP FOR RT PURPOSES 
        % draw rectangles
        Screen('FillRect', screen_ptr, rect_fill, r1_rect);
        Screen('FillRect', screen_ptr, rect_fill, r2_rect);
        Screen('FillRect', screen_ptr, rect_fill, r3_rect);

        % DRAW CIRCLES
        Screen('FillOval', screen_ptr , circle_fill, circle1_square_tmp);
        Screen('FillOval', screen_ptr , circle_fill, circle2_square_tmp);
        Screen('FillOval', screen_ptr , circle_fill, circle3_square_tmp);

        % draw submit button
        % initially make gray
        Screen('FrameRect', screen_ptr, black, submit_rect, submit_rect_thick);
        RDCL_DrawText(submit_rect_c(1), submit_rect_c(2), "SUBMIT", 'Col', rect_fill);

        % draw labels
        RDCL_DrawText(r1_txt_x, r1_txt_y, "1", 'Col', black);
        RDCL_DrawText(r2_txt_x, r2_txt_y, "2", 'Col', black);
        RDCL_DrawText(r3_txt_x, r3_txt_y, "3", 'Col', black);

        RDCL_DrawText(c1_txt_x, c1_txt_y, "1", 'Col', black);
        RDCL_DrawText(c2_txt_x, c2_txt_y, "2", 'Col', black);
        RDCL_DrawText(c3_txt_x, c3_txt_y, "3", 'Col', black);

        % SHOW EVERYTHING 
        [trial_onset_time] = Screen('Flip', screen_ptr);
      
        % ADJUSTMENT PHASE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        % keep track of whether ppt is still adjusting
        adjusting=true;

        % INIT variable keeping track of last time the mouse button was up
        last_mouse_up = [0 0];
        while adjusting
          
          % CHECK CURRENT MOUSE POS
          [click_x click_y buttons] = GetMouse(screen_ptr);

          % COMPUTE CURRENT RT
          rt_tmp = GetSecs()-trial_onset_time;
          
          % CHECK IF MOUSE BUTTON DOWN
          if buttons(1,1) 

            % BUTTON DOWN IN CIRCLE 1 SQUARE & LAST TIME UP IN CIRCLE 1 SQUARE
            if click_x >= circle1_square(1) && click_x < circle2_square(1) && click_y<=circle1_square(4) && last_mouse_up(1) >= circle1_square(1) && last_mouse_up(1) < circle2_square(1) 
              circle1_rad_start = [circle1_square_x_center circle1_square_y_center];
              circle1_rad_end = [click_x click_y];
              circle1_rad_tmp = compute_distance(circle1_rad_start, circle1_rad_end); 
              circle1_area_tmp=area_from_rad(circle1_rad_tmp);
              if circle1_area_tmp > max_circle_area
                circle1_rad_tmp = rad_from_area(max_circle_area);
                circle1_area_tmp = max_circle_area;
              elseif circle1_area_tmp < min_circle_area
                circle1_rad_tmp = min_circle_rad;
                circle1_area_tmp = min_circle_area;
              endif
            
              % "square" where we draw the circle
                circle1_square_tmp = [
                  circle1_square_x_center-circle1_rad_tmp
                  circle1_square_y_center-circle1_rad_tmp
                  circle1_square_x_center+circle1_rad_tmp
                  circle1_square_y_center+circle1_rad_tmp
              ];
            
            % BUTTON DOWN IN CIRCLE 2 SQUARE & LAST TIME UP IN CIRCLE 2 SQUARE 
            elseif  click_x >= circle2_square(1) && click_x < circle3_square(1) && click_y<=circle2_square(4) && last_mouse_up(1) >= circle2_square(1) && last_mouse_up(1) < circle3_square(1) 
              circle2_rad_start = [circle2_square_x_center circle2_square_y_center];
              circle2_rad_end = [click_x click_y];
              circle2_rad_tmp = compute_distance(circle2_rad_start, circle2_rad_end); 
              circle2_area_tmp=area_from_rad(circle2_rad_tmp);
              if circle2_area_tmp > max_circle_area
                circle2_rad_tmp = rad_from_area(max_circle_area);
                circle2_area_tmp = max_circle_area;
              elseif circle2_area_tmp < min_circle_area
                circle2_rad_tmp = min_circle_rad;
                circle2_area_tmp = min_circle_area;
              endif

              % "square" where we draw the circle
              circle2_square_tmp = [
                circle2_square_x_center-circle2_rad_tmp
                circle2_square_y_center-circle2_rad_tmp
                circle2_square_x_center+circle2_rad_tmp
                circle2_square_y_center+circle2_rad_tmp
            ];
                          
            % BUTTON DOWN IN CIRCLE 3 SQUARE & LAST TIME UP IN CIRCLE 3 SQUARE
            elseif click_x >= circle3_square(1) && click_x < circle3_square(3) &&click_y<=circle3_square(4) &&last_mouse_up(1) >= circle3_square(1) &&last_mouse_up(1) < circle3_square(3) 

              circle3_rad_start = [circle3_square_x_center circle3_square_y_center];
              circle3_rad_end = [click_x click_y];
              circle3_rad_tmp = compute_distance(circle3_rad_start, circle3_rad_end); 
              circle3_area_tmp=area_from_rad(circle3_rad_tmp);
              if circle3_area_tmp > max_circle_area
                  circle3_rad_tmp = rad_from_area(max_circle_area);
                  circle3_area_tmp = max_circle_area;
              elseif circle3_area_tmp < min_circle_area
                  circle3_rad_tmp = min_circle_rad;
                  circle3_area_tmp = min_circle_area;
              endif
            
              % "square" where we draw the circle
              circle3_square_tmp = [
                circle3_square_x_center-circle3_rad_tmp
                circle3_square_y_center-circle3_rad_tmp
                circle3_square_x_center+circle3_rad_tmp
                circle3_square_y_center+circle3_rad_tmp
              ];
            
            % BUTTON DOWN IN SUBMIT BOX & LAST TIME UP IN SUBMIT BOX
            elseif click_x >= submit_rect(1) &&click_x < submit_rect(3) &&click_y < submit_rect(4) &&click_y >= submit_rect(2) &&rt_tmp >=submit_min &&last_mouse_up(1) >= submit_rect(1) &&last_mouse_up(1) < submit_rect(3) &&last_mouse_up(2) < submit_rect(4) &&last_mouse_up(2) >= submit_rect(2)
              
              r1_diff = (h1_tmp*w1_tmp)-circle1_area_tmp;
              r2_diff = (h2_tmp*w2_tmp)-circle2_area_tmp;
              r3_diff = (h3_tmp*w3_tmp)-circle3_area_tmp;
              circle_error = [circle_error(1:trial_index-1) r1_diff r2_diff r3_diff];
              fprintf(fp_circle, 
              '%s, %d, %s, %s, %s, %d, %d, %d, %d, %d, %d, %d, %d, %d, %d, %d, %d, %d, %d, %d, %d, %d\n',
              sub_n, computer_num, disp_cond, effect_tmp, set_tmp, distance_tmp, diag_tmp, block_num, trial_index, 
              h1_tmp, w1_tmp, h2_tmp, w2_tmp, h3_tmp, w3_tmp, circle1_area_tmp, circle1_rad_tmp, circle2_area_tmp, circle2_rad_tmp, circle3_area_tmp, circle3_rad_tmp, rt_tmp);
              fflush(fp_circle);
              adjusting=false;

        endif          % MOUSE UP  
          elseif ~any(buttons)
            last_mouse_up = [click_x click_y];
        endif
        
      % clear screen
      Screen('FillRect',screen_ptr,white);
      % draw rectangles
      Screen('FillRect', screen_ptr, rect_fill, r1_rect);
      Screen('FillRect', screen_ptr, rect_fill, r2_rect);
      Screen('FillRect', screen_ptr, rect_fill, r3_rect);
      % DRAW CIRCLES
      Screen('FillOval', screen_ptr , circle_fill, circle1_square_tmp);
      Screen('FillOval', screen_ptr , circle_fill, circle2_square_tmp);
      Screen('FillOval', screen_ptr , circle_fill, circle3_square_tmp);

      % draw submit button
      Screen('FrameRect', screen_ptr, black, submit_rect, submit_rect_thick);
      if(rt_tmp>=submit_min)
        RDCL_DrawText(submit_rect_c(1), submit_rect_c(2), "SUBMIT", 'Col', black);
      else
        RDCL_DrawText(submit_rect_c(1), submit_rect_c(2), "SUBMIT", 'Col', rect_fill);
      endif 
      % draw labels
      RDCL_DrawText(r1_txt_x, r1_txt_y, "1", 'Col', black);
      RDCL_DrawText(r2_txt_x, r2_txt_y, "2", 'Col', black);
      RDCL_DrawText(r3_txt_x, r3_txt_y, "3", 'Col', black);
      
      RDCL_DrawText(c1_txt_x, c1_txt_y, "1", 'Col', black);
      RDCL_DrawText(c2_txt_x, c2_txt_y, "2", 'Col', black);
      RDCL_DrawText(c3_txt_x, c3_txt_y, "3", 'Col', black);
      % SHOW EVERYTHING
      Screen('Flip', screen_ptr);
      WaitSecs(lag_time);
    endwhile
  endfor
endfor

% START CHOICE PHASE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Create/open circle area output file
fp_choice = fopen(fullfile(choice_destination_folder,choice_output_name),'w');     
% Write to circle area output file
fprintf(fp_choice, 'sub_n, computer_n, disp_cond, effect, set, distance, diag, block_n, trial_n, h1, w1, h2, w2, h3, w3, choice, rt \n');

% READ IN CHOICE TRIAL PARAMETERS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
% NUMERIC PARAMS
h1 = dlmread("choice_trial_params/h1.txt");
h2 = dlmread("choice_trial_params/h2.txt");
h3 = dlmread("choice_trial_params/h3.txt");
w1 = dlmread("choice_trial_params/w1.txt");
w2 = dlmread("choice_trial_params/w2.txt");
w3 = dlmread("choice_trial_params/w3.txt");
distance = dlmread("choice_trial_params/distance.txt");
diag = dlmread("choice_trial_params/diag.txt");

% STRING PARAMS
sets = read_params("choice_trial_params/set.txt","%s");
effect = read_params("choice_trial_params/effect.txt","%s");

% DETERMINE N TRIALS
n_choice_trials=numel(h1);


% quadrant locs for drawing rectangles and circles
rect_quadrant = [s_middle_x-(s_middle_x/2) s_middle_y-(s_middle_y/2) s_middle_x+(s_middle_x/2) s_middle_y+(s_middle_y/2)];
% actual "rectangle" where rectangles go
rectangle_box_c = [s_middle_x s_middle_y];
rectangle_box = CenterRectOnPoint(rect_quadrant, s_middle_x, s_middle_y);

% width && height of the rectangle box
rectangle_box_w = rectangle_box(3)-rectangle_box(1);
rectangle_box_h = rectangle_box(4)-rectangle_box(2);

% CHOICE INSTRUCITIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Screen('FillRect', screen_ptr, white);
RDCL_DrawText(s_middle_x, s_middle_y-300, "You finished the first phase of the experiment!", 'Col', black);
RDCL_DrawText(s_middle_x, s_middle_y-250, "We will now begin the second (and final) phase of the experiment.", 'Col', black);
RDCL_DrawText(s_middle_x, s_middle_y-150, "Press 'space' to continue the instructions.", 'Col', black);
Screen('Flip', screen_ptr); 
RDCL_GetResponse({{'space', 0}});

Screen('FillRect', screen_ptr, white);
RDCL_DrawText(s_middle_x, s_middle_y-300, "On each trial, you will again see 3 rectangles on the screen, labeled ‘1’, ‘2’, and ‘3’.  ", 'Col', black);
RDCL_DrawText(s_middle_x, s_middle_y-250, "YOUR JOB IS TO SELECT THE RECTANGLE WITH THE LARGEST AREA.", 'Col', black);
RDCL_DrawText(s_middle_x, s_middle_y-200, "You select a rectangle using the mouse and left mouse button.", 'Col', black);
RDCL_DrawText(s_middle_x, s_middle_y-100, "Press ‘space’ to continue the instructions.", 'Col', black);
Screen('Flip', screen_ptr); 
RDCL_GetResponse({{'space', 0}});

Screen('FillRect', screen_ptr, white);
RDCL_DrawText(s_middle_x, s_middle_y-300, "Some trials will be harder than others. We only ask that you try your best.", 'Col', black);
RDCL_DrawText(s_middle_x, s_middle_y-250, "We will keep track of how you did and let you know at the end of the experiment.", 'Col', black);
RDCL_DrawText(s_middle_x, s_middle_y-150, "Press ‘space’ to continue the instructions.", 'Col', black);
Screen('Flip', screen_ptr); 
RDCL_GetResponse({{'space', 0}});

Screen('FillRect', screen_ptr, white);
RDCL_DrawText(s_middle_x, s_middle_y-300, "We will start with a few practice trials", 'Col', black);
RDCL_DrawText(s_middle_x, s_middle_y-200, "Press ‘space’ to start the practice trials", 'Col', black);
Screen('Flip', screen_ptr); 
RDCL_GetResponse({{'space', 0}});



for trial_index = 1:n_choice_prac
      SetMouse(s_middle_x + add_noise(s_middle_x/2), s_rect(4)*.9);

      Screen('FillRect', screen_ptr, white);
      Screen('Flip', screen_ptr); 
      WaitSecs(.1);

      % get set
      set_tmp = "NA";
      distance_tmp = 999;
      effect_tmp = "practice"
      diag_tmp = 999;
      block_num=0;

      r1_tmp = sample_filler(min_stim,max_stim);
      r2_tmp = sample_filler(min_stim,max_stim);
      r3_tmp = sample_filler(min_stim,max_stim);
      w_all_tmp_unshuffled = [r1_tmp(1) r2_tmp(1) r3_tmp(1)];
      h_all_tmp_unshuffled = [r1_tmp(2) r2_tmp(2) r3_tmp(2)];


      % shuffle rects 
      rects_order_tmp = RDCL_RandomizeTrials(3);

      % assigned shuffled rects to h1, h2, h3
      h1_tmp = round(h_all_tmp_unshuffled(rects_order_tmp(1)));
      h2_tmp = round(h_all_tmp_unshuffled(rects_order_tmp(2)));
      h3_tmp = round(h_all_tmp_unshuffled(rects_order_tmp(3)));

      % assigned shuffled rects to w1, w2, w3
      w1_tmp = round(w_all_tmp_unshuffled(rects_order_tmp(1)));
      w2_tmp = round(w_all_tmp_unshuffled(rects_order_tmp(2)));
      w3_tmp = round(w_all_tmp_unshuffled(rects_order_tmp(3)));

      % array of all heights
      h_all_tmp_shuffled = [h1_tmp h2_tmp h3_tmp];

      % array of all widths
      w_all_tmp_shuffled = [w1_tmp w2_tmp w3_tmp];

      % Rectangle 1 center x pos
      r1_x = rectangle_box_c(1)-(.5*w1_tmp)-(rect_dist_px)-(.5*w2_tmp);
      
      % Rectangle 2 center x pos
      r2_x = rectangle_box_c(1);
      % rectangle 3 center x pos
      r3_x = rectangle_box_c(1)+(.5*w2_tmp)+(rect_dist_px)+(.5*w3_tmp);

      % HORIZONTAL (Trueblood style) display %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    if strcmp(disp_cond,"horizontal")
      
      r1_y = rectangle_box_c(2)+r1_jit;
      r2_y = rectangle_box_c(2)+r2_jit;
      r3_y = rectangle_box_c(2)+r3_jit;
      

    % TRIANGLE (Spektor style) display    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    elseif strcmp(disp_cond, "triangle")
        r1_y = round(rectangle_box_c(2)+(.5*rect_dist_px)+.5*h1_tmp+r1_jit);
        r2_y = round(rectangle_box_c(2)-(.5*rect_dist_px)-.5*h2_tmp+r2_jit);
        r3_y = round(rectangle_box_c(2)+(.5*rect_dist_px)+.5*h3_tmp+r3_jit);           
    endif 
     % RECRTANGLE LABELS
        rtxt_y = max([round(r1_y+h1_tmp)
                      round(r2_y+h2_tmp)
                      round(r3_y+h3_tmp)])+round(.05*rectangle_box_h);
        
        % Rectangle 1 text center
        r1_txt_x = r1_x;
  
        r1_txt_y = rtxt_y;
        % Rectangle 2 text center
        r2_txt_x = r2_x;
        r2_txt_y = rtxt_y;

        % Rectangle 3 text center
        r3_txt_x = r3_x;
        r3_txt_y = rtxt_y;        
        
    % Center rectangles 
    r1_rect = CenterRectOnPoint([0 0 w1_tmp h1_tmp], r1_x, r1_y);
    r2_rect = CenterRectOnPoint([0 0 w2_tmp h2_tmp], r2_x, r2_y);
    r3_rect = CenterRectOnPoint([0 0 w3_tmp h3_tmp], r3_x, r3_y);
    
    % INITIAL STIMULUS PRESENTATION
    % draw rectangles
    Screen('FillRect', screen_ptr, rect_fill, r1_rect);
    Screen('FillRect', screen_ptr, rect_fill, r2_rect);
    Screen('FillRect', screen_ptr, rect_fill, r3_rect);
    % draw labels
    RDCL_DrawText(r1_txt_x, r1_txt_y, "1", 'Col', black);
    RDCL_DrawText(r2_txt_x, r2_txt_y, "2", 'Col', black);
    RDCL_DrawText(r3_txt_x, r3_txt_y, "3", 'Col', black);
    RDCL_DrawText(s_middle_x, s_rect(4)*.25, "Click on the rectangle with the LARGEST area", 'Col', black);
    
    % SHOW EVERYTHING
    [trial_onset_time] = Screen('Flip', screen_ptr);

    choosing=true;
    while choosing
    [clicks click_x click_y] = GetClicks(screen_ptr);
      if click_x > r1_rect(1) && click_x <= r1_rect(3) && click_y > r1_rect(2) && click_y<=r1_rect(4)
        choice_tmp = 1;
        rt_tmp = GetSecs()-trial_onset_time;        
        choosing=false;
      elseif click_x > r2_rect(1) && click_x <= r2_rect(3) && click_y > r2_rect(2) && click_y<=r2_rect(4)
        choice_tmp = 2;
        rt_tmp = GetSecs()-trial_onset_time;
        choosing=false;
      elseif click_x > r3_rect(1) && click_x <= r3_rect(3) && click_y > r3_rect(2) && click_y<=r3_rect(4)
        choice_tmp = 3;
        rt_tmp = GetSecs()-trial_onset_time;
        choosing=false;
    endif
endwhile

        fprintf(fp_choice, 
        '%s, %d, %s, %s, %s, %d, %d, %d, %d, %d, %d, %d, %d, %d, %d, %d, %d\n',
        sub_n, computer_num, disp_cond, effect_tmp, set_tmp, distance_tmp, diag_tmp, block_num, trial_index, 
        h1_tmp, w1_tmp, h2_tmp, w2_tmp, h3_tmp, w3_tmp, choice_tmp, rt_tmp);
fflush(fp_choice);
endfor

Screen('FillRect', screen_ptr, white);
RDCL_DrawText(s_middle_x, s_middle_y-300, "You finished the practice trials.", 'Col', black);
RDCL_DrawText(s_middle_x, s_middle_y-200, "Press ‘space’ to start the experimental trials", 'Col', black);
Screen('Flip', screen_ptr); 
RDCL_GetResponse({{'space', 0}});

choice_correct = 0;
choice_total = n_choice_trials*n_choice_blocks;
for block_num = 1:n_choice_blocks
  trial_order = RDCL_RandomizeTrials(n_choice_trials);
  for trial_index = 1:n_choice_trials
    SetMouse(s_middle_x + add_noise(s_middle_x/2), s_rect(4)*.9);

    Screen('FillRect', screen_ptr, white);
    Screen('Flip', screen_ptr); 
    WaitSecs(.1);
    % Get shuffled trial number
    trial_num = trial_order(trial_index);

    % get set
    set_tmp = sets{trial_num};
    distance_tmp = distance(trial_num);
    effect_tmp = effect{trial_num};
    diag_tmp = diag(trial_num);

    % GET RECT HEIGHT & WIDTH
    if strcmp(effect_tmp,"catch") 
      r1_tmp = sample_diag(d1_r, d1_int);
      r2_tmp = sample_diag(d1_r, d1_int);
      r3_tmp = sample_diag(d3_r, d3_int);
      w_all_tmp_unshuffled = [r1_tmp(1) r2_tmp(1) r3_tmp(1)];
      h_all_tmp_unshuffled = [r1_tmp(2) r2_tmp(2) r3_tmp(2)];
    elseif strcmp(effect_tmp,"filler")
      r1_tmp = sample_filler(min_stim,max_stim);
      r2_tmp = sample_filler(min_stim,max_stim);
      r3_tmp = sample_filler(min_stim,max_stim);
      w_all_tmp_unshuffled = [r1_tmp(1) r2_tmp(1) r3_tmp(1)];
      h_all_tmp_unshuffled = [r1_tmp(2) r2_tmp(2) r3_tmp(2)];
    elseif strcmp(effect_tmp,"attraction")
        h_all_tmp_unshuffled = [h1(trial_num) h2(trial_num) h3(trial_num)];
        w_all_tmp_unshuffled = [w1(trial_num) w2(trial_num) w3(trial_num)];
    endif

    % shuffle rects 
    rects_order_tmp = RDCL_RandomizeTrials(3);

    % assigned shuffled rects to h1, h2, h3
    h1_tmp = round(h_all_tmp_unshuffled(rects_order_tmp(1)));
    h2_tmp = round(h_all_tmp_unshuffled(rects_order_tmp(2)));
    h3_tmp = round(h_all_tmp_unshuffled(rects_order_tmp(3)));

    % assigned shuffled rects to w1, w2, w3
    w1_tmp = round(w_all_tmp_unshuffled(rects_order_tmp(1)));
    w2_tmp = round(w_all_tmp_unshuffled(rects_order_tmp(2)));
    w3_tmp = round(w_all_tmp_unshuffled(rects_order_tmp(3)));

    % array of all heights
    h_all_tmp_shuffled = [h1_tmp h2_tmp h3_tmp];

    % array of all widths
    w_all_tmp_shuffled = [w1_tmp w2_tmp w3_tmp];

    % Rectangle 1 center x pos
    r1_x = rectangle_box_c(1)-(.5*w1_tmp)-(rect_dist_px)-(.5*w2_tmp);
    % Rectangle 2 center x pos
    r2_x = rectangle_box_c(1);
    % rectangle 3 center x pos
    r3_x = rectangle_box_c(1)+(.5*w2_tmp)+(rect_dist_px)+(.5*w3_tmp);

    % HORIZONTAL (Trueblood style) display %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  if strcmp(disp_cond,"horizontal")
    
    r1_y = rectangle_box_c(2)+r1_jit;
    r2_y = rectangle_box_c(2)+r2_jit;
    r3_y = rectangle_box_c(2)+r3_jit;
    

  % TRIANGLE (Spektor style) display    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  elseif strcmp(disp_cond, "triangle")
      r1_y = round(rectangle_box_c(2)+(.5*rect_dist_px)+.5*h1_tmp+r1_jit);
      r2_y = round(rectangle_box_c(2)-(.5*rect_dist_px)-.5*h2_tmp+r2_jit);
      r3_y = round(rectangle_box_c(2)+(.5*rect_dist_px)+.5*h3_tmp+r3_jit);           
  endif 
   % RECRTANGLE LABELS
        rtxt_y = max([round(r1_y+h1_tmp)
                      round(r2_y+h2_tmp)
                      round(r3_y+h3_tmp)])+round(.05*rectangle_box_h);
            RDCL_DrawText(s_middle_x, s_rect(4)*.25, "Click on the rectangle with the LARGEST area", 'Col', black);

        % Rectangle 1 text center
        r1_txt_x = r1_x;
  
        r1_txt_y = rtxt_y;
        % Rectangle 2 text center
        r2_txt_x = r2_x;
        r2_txt_y = rtxt_y;

        % Rectangle 3 text center
        r3_txt_x = r3_x;
        r3_txt_y = rtxt_y;
        
          % Center rectangles 
          r1_rect = CenterRectOnPoint([0 0 w1_tmp h1_tmp], r1_x, r1_y);
          r2_rect = CenterRectOnPoint([0 0 w2_tmp h2_tmp], r2_x, r2_y);
          r3_rect = CenterRectOnPoint([0 0 w3_tmp h3_tmp], r3_x, r3_y);
    % INITIAL STIMULUS PRESENTATION
  % draw rectangles
  Screen('FillRect', screen_ptr, rect_fill, r1_rect);
  Screen('FillRect', screen_ptr, rect_fill, r2_rect);
  Screen('FillRect', screen_ptr, rect_fill, r3_rect);
  % draw labels
  RDCL_DrawText(r1_txt_x, r1_txt_y, "1", 'Col', black);
  RDCL_DrawText(r2_txt_x, r2_txt_y, "2", 'Col', black);
  RDCL_DrawText(r3_txt_x, r3_txt_y, "3", 'Col', black);
    
  % SHOW EVERYTHING
  [trial_onset_time] = Screen('Flip', screen_ptr);

  choosing=true;
  while choosing
  [clicks click_x click_y] = GetClicks(screen_ptr);
  if click_x > r1_rect(1) && click_x <= r1_rect(3) && click_y > r1_rect(2) && click_y<=r1_rect(4)
      choice_tmp = 1;
      choice_area_tmp =h1_tmp*w1_tmp;
      rt_tmp = GetSecs()-trial_onset_time;
      choosing=false;
    elseif click_x > r2_rect(1) && click_x <= r2_rect(3) && click_y > r2_rect(2) && click_y<=r2_rect(4)
      choice_tmp = 2;
      choice_area_tmp =h2_tmp*w2_tmp;
      rt_tmp = GetSecs()-trial_onset_time;
      choosing=false;
    elseif click_x > r3_rect(1) && click_x <= r3_rect(3) && click_y > r3_rect(2) && click_y<=r3_rect(4)
      choice_tmp = 3;
      choice_area_tmp =h3_tmp*w3_tmp;
      rt_tmp = GetSecs()-trial_onset_time;
      choosing=false;
   endif
  endwhile
  
      fprintf(fp_choice, 
  '%s, %d, %s, %s, %s, %d, %d, %d, %d, %d, %d, %d, %d, %d, %d, %d, %d\n',
  sub_n, computer_num, disp_cond, effect_tmp, set_tmp, distance_tmp, diag_tmp, block_num, trial_index, 
  h1_tmp, w1_tmp, h2_tmp, w2_tmp, h3_tmp, w3_tmp, choice_tmp, rt_tmp);
  fflush(fp_choice);
  if choice_area_tmp == max([h1_tmp*w1_tmp h2_tmp*w2_tmp h3_tmp*w3_tmp])
    choice_correct=choice_correct+1
  endif
  endfor
endfor
choice_correct_perc = round((choice_correct/choice_total)*100);

Screen('FillRect', screen_ptr, white);
RDCL_DrawText(s_middle_x, s_middle_y-300, "Congratulations! You reached the end of the experiment", 'Col', black);
RDCL_DrawText(s_middle_x, s_middle_y-250, "Your percentage correct on the choice trials was:", 'Col', black);
RDCL_DrawText(s_middle_x, s_middle_y-200, strcat(num2str(choice_correct_perc), "%"), 'Col', black);
RDCL_DrawText(s_middle_x, s_middle_y-100, "Press ‘space’ to see the debriefing form.", 'Col', black);
Screen('Flip', screen_ptr); 
RDCL_GetResponse({{'space', 0}});

debrief_info = imfinfo('Judg_choice_debrief.png');
debrief_width = debrief_info.Width;
debrief_height = debrief_info.Height;

%Get the image for instructions
debrief = imread('Judg_choice_debrief.png');

%Where and how big should the image be?
debrief_scale = .55;
debrief_rect = [s_middle_x-round(debrief_scale*debrief_width/1.5) 
      s_middle_y-round(debrief_scale*debrief_height/1.5) 
      s_middle_x+round(debrief_scale*debrief_width/1.5) 
      s_middle_y+round(debrief_scale*debrief_height/1.5)];

% Put the image into the buffer
Screen('PutImage', screen_ptr, debrief, debrief_rect);

%Open Screen with instructions displayed. When they hit space the next page shows up
Screen('Flip', screen_ptr);
RDCL_GetResponse({{'space', 0}}); 
sca
end
