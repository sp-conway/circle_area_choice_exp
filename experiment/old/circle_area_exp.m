% Fall 2023
% Circle-adjustment CE exp
function circle_area_exp
   
      more off;
      
      global screen_ptr;
      
      current_points=0;
      
      %Add path to RDCL_Functions
      addpath('./RDCL_Functions');    
      
      %Add path to utility functions
      addpath('./utility_functions');
      
      % add path to trial params
      addpath('./trial_params');
      
      % READ IN TRIAL PARAMETERS
      h1 = read_params("trial_params/h1.txt","%d");
      w1 = read_params("trial_params/w1.txt","%d");
      h2 = read_params("trial_params/h2.txt","%d");
      w2 = read_params("trial_params/w2.txt","%d");
      h3 = read_params("trial_params/h3.txt","%d");
      w3 = read_params("trial_params/w3.txt","%d");
      sets = read_params("trial_params/set.txt","%s");
      distance = read_params("trial_params/distance.txt","%d");
      effect = read_params("trial_params/effect.txt","%s");
      diag = read_params("trial_params/diag.txt","%d");
      

      % CONSTANT VARIABLES %

      % min circle area & radius
      min_circle_area = 1000;
      min_circle_rad = rad_from_area(min_circle_area);

      % NUMBER OF BLOCKS (CHANGE!!!!!!)
      n_blocks = 4;

      %% vertical jitter
      vjit = 15; 

      % rect gray
      rect_fill = [87 88 91];

      % for now circle is same color as rectangles
      circle_fill = rect_fill;

      % rect thickness
      rect_thick = 1;
      
      % circle size (avg)
      mean_circle_area = 2500;

      % max circle location noise
      max_circle_noise = 100;

      % max circle area noise
      max_circle_area_noise = 1200;

      % arrow width
      arrow_width = 40;

      % little adjustments
      adj_left = -1;
      adj_right = 1;

      % big adjustments
      adj_down = -5;
      adj_up = 5;
      
      % rectangle distance
      rect_dist = .12;

      % init points variable
      total_points = 0;
           
      %Create save path for output
      destination_folder = './data';
      addpath(destination_folder);
      
      %KB NAME
      KbName('UnifyKeyNames');
      
      % Prompt text
      prompt = ('Please enter the participant number:');
      
      % input dialog
      answer = inputdlg(prompt);
      sub_n = deal(answer{:});
      sub_n = num2str(sub_n);
      output_name = ['circle_area_exp_' sub_n '.csv'];
      
     % Check to see if output name exists
      while exist(output_name)
        sub_n_error = ('That participant number already exists. Please double check the run sheet and enter a new number.');
        error_message = inputdlg(sub_n_error);
        sub_n = deal(error_message{:});
        sub_n = num2str(sub_n);
        output_name = ['circle_area_exp_ppt_' sub_n '.csv'];
      end 

      % get condition (odd subject # means trueblood style, even subject # means spektor style)
      if rem(str2double(sub_n),2)>0
        disp_cond = "horizontal";
      else
        disp_cond = "triangle";
      endif 
      

      % Create/open output file
      fp = fopen(fullfile(destination_folder,output_name),'w');     

      % Write to output file
      fprintf(fp, 'sub_n, disp_cond, effect, set, distance, diag, block_num, trial_index, probe_rect, h1, w1, h2, w2, h3, w3, circle_area, circle_rad, resp_key, resp_num,  adj, current_points, total_points, time_secs, rt_current, rt_total \n');
      % Dummy calls    
      RDCL_DummyCalls(); 
      
      % seed rng
      RDCL_SeedRandomNumbers();

      % Open Screen  
      [screen_ptr s_rect] = Screen('OpenWindow', 0, 0, [], 32, 2);
      disp(screen_ptr);

      S.middle_x=s_rect(2)*.5;
      S.middle_y=s_rect(4)*.5;
      
      % white
      white = [255 255 255];

      % black
      black = [0 0 0];

      quadrant = [0 round(s_rect(4))*.5 round(s_rect(3)*.5) round(s_rect(4))];

      % get location for circle center
      % This location remains the same for 1, 2, & 3 option trials, but will be jittered on each trial
      circle_x_loc = round(.75*s_rect(3));
      circle_y_loc = round(s_rect(4)*.25);
      circle_box = CenterRectOnPointd(quadrant, circle_x_loc, circle_y_loc);
      rectangle_box_c = [round(.25*s_rect(3)) round(.75*s_rect(4))];
      rectangle_box = CenterRectOnPointd(quadrant, rectangle_box_c(1), rectangle_box_c(2));
 
      % width & height of the rectangle box
      rectangle_box_w = rectangle_box(3)-rectangle_box(1);
      rectangle_box_h = rectangle_box(4)-rectangle_box(2);

      % y location of probe arrow
      probe_arrow_y = s_rect(4)*.10;

      % max circle area & radius
      max_circle_area = 60000;
      max_circle_rad = rad_from_area(max_circle_area);

      % number of trials (can calc based on any dimension, I picked h1 arbitrarily)
      n_trials = numel(h1);

      % Shuffle trials
      trial_order = RDCL_RandomizeTrials(n_trials);
      
      Screen('FillRect',screen_ptr,white);
      Screen('Flip',screen_ptr);

      % calibration trial
      for block_num = 1:n_blocks

          % loop through trials
          for trial_index = 1:n_trials 
    
            % Get shuffled trial number
            trial_num = trial_order(trial_index);

            % get set
            set_tmp = sets(trial_num);
            distance_tmp = distance(trial_num);
            effect_tmp = effect(trial_num);
            diag_tmp = diag(trial_num);

            if strcmp(effect_tmp,"catch") 
                if strcmp(set_tmp,"catch1") 
                    h_all_tmp_unshuffled = [unifrnd(50,80) 0 0 ];
                    w_all_tmp_unshuffled = [unifrnd(50,80) 0 0 ];
                elseif strcmp(set_tmp,"catch2")
                    h_all_tmp_unshuffled = [unifrnd(50,80) unifrnd(25,50) 0];
                    w_all_tmp_unshuffled = [unifrnd(50,80) unifrnd(25,50) 0 ];
                else 
                    h_all_tmp_unshuffled = [unifrnd(50,80) unifrnd(25,50) unifrnd(25,50)];
                    w_all_tmp_unshuffled = [unifrnd(50,80) unifrnd(25,50) unifrnd(25,50)];
                endif
            else 
                % heights (unshuffled)
                h_all_tmp_unshuffled = [h1(trial_num) h2(trial_num) h3(trial_num)];
                w_all_tmp_unshuffled = [w1(trial_num) w2(trial_num) w3(trial_num)];
            endif 
              
    
            % shuffle rects (always shuffle 3, even if there aren't)
            rects_order_tmp = RDCL_RandomizeTrials(3);
    
            % assigned shuffled rects to h1, h2, h3
            h1_tmp = h_all_tmp_unshuffled(rects_order_tmp(1));
            h2_tmp = h_all_tmp_unshuffled(rects_order_tmp(2));
            h3_tmp = h_all_tmp_unshuffled(rects_order_tmp(3));
    
            % assigned shuffled rects to w1, w2, w3
            w1_tmp = w_all_tmp_unshuffled(rects_order_tmp(1));
            w2_tmp = w_all_tmp_unshuffled(rects_order_tmp(2));
            w3_tmp = w_all_tmp_unshuffled(rects_order_tmp(3));
    
            % array of all heights
            h_all_tmp_shuffled = [h1_tmp h2_tmp h3_tmp];
    
            % array of all widths
            w_all_tmp_shuffled = [w1_tmp w2_tmp w3_tmp];
    
            % figure out where we are to probe
            probe_rects = find(h_all_tmp_shuffled>0);
    
            % shuffle probe order
            probe_rects_shuffled = shuffle(probe_rects);
    
            % number of probes
            n_probe = numel(probe_rects_shuffled);
                
            % HORIZONTAL (Trueblood style) display            
            if strcmp(disp_cond,"horizontal")
    
                % Rectangle 1 center
                r1_x = rectangle_box_c(1) - (.5*w1_tmp) - (rect_dist * rectangle_box_w) - (.5*w2_tmp);
                r1_y = add_noise(rectangle_box_c(2),vjit);
    
                % Rectangle 2 center
                r2_x = rectangle_box_c(1);
                r2_y = add_noise(rectangle_box_c(2), vjit);
    
                % Rectangle 3 center
                r3_x = rectangle_box_c(1) + (.5*w2_tmp) + (rect_dist * rectangle_box_w) + (.5*w3_tmp);
                r3_y = add_noise(rectangle_box_c(2), vjit);

    
            % TRIANGLE (Spektor style) display    
            else 
                % Rectangle 1 center
                r1_x = rectangle_box_c(1) - (.5*w1_tmp) - (rect_dist * rectangle_box_w) - (.5*w2_tmp);
                r1_y = add_noise(rectangle_box_c(2) -.5*h1_tmp,vjit);
    
                % Rectangle 2 center
                r2_x = rectangle_box_c(1);
                % add_noise(center[1]-(.5*( (h1+h3)/2 )) -(rect_dist*canvas_height)-h2)
                r2_y = add_noise(rectangle_box_c(2) - ( .5*( ( h1_tmp+h3_tmp )/2 ) ) - (rect_dist*rectangle_box_h) - h2_tmp, vjit);
    
                % Rectangle 3 center
                r3_x = rectangle_box_c(1) + (.5*w2_tmp) + (rect_dist * rectangle_box_w) + (.5*w3_tmp);
                r3_y = add_noise(rectangle_box_c(2) -.5*h3_tmp,vjit);
    
          end 
          
              % figure out text loc
              % Rectangle 1 text center
              r1_txt_x = r1_x;
              txt_dist_y = max([round(r1_y+h1_tmp)
                             round(r2_y+h2_tmp) 
                             round(r3_y+h3_tmp)])+.025*rectangle_box_h;
              
              r1_txt_y = txt_dist_y;
              % Rectangle 2 text center
              r2_txt_x = r2_x;
              r2_txt_y = txt_dist_y;
  
              % Rectangle 3 text center
              r3_txt_x = r3_x;
              r3_txt_y = txt_dist_y;
  
              % figure out colors
              if h1_tmp==0
                  r1_fill = white;
                  r1_border = white;
              else 
                  r1_fill = rect_fill;
                  r1_border = black;
              end 
  
              if h2_tmp==0
                  r2_fill = white;
                  r2_border = white;
              else 
                  sca;
                  r2_fill = rect_fill;
                  r2_border = black;
              end 
  
              if h3_tmp==0
                  r3_fill = white;
                  r3_border = white;
              else 
                  r3_fill = rect_fill;
                  r3_border = black;
              end 
  
                % Center rectangles (Draw both inner and outer)
                r1_outer = CenterRectOnPointd([0 0 w1_tmp h1_tmp], r1_x, r1_y);
                r2_outer = CenterRectOnPointd([0 0 w2_tmp h2_tmp], r2_x, r2_y);
                r3_outer = CenterRectOnPointd([0 0 w3_tmp h3_tmp], r3_x, r3_y);
                r1_inner = CenterRectOnPointd([0 0 w1_tmp-rect_thick h1_tmp-rect_thick], r1_x, r1_y);
                r2_inner = CenterRectOnPointd([0 0 w2_tmp-rect_thick h2_tmp-rect_thick], r2_x, r2_y);
                r3_inner = CenterRectOnPointd([0 0 w3_tmp-rect_thick h3_tmp-rect_thick], r3_x, r3_y);
                
                % loop through rect probes
                for adj_phase = 1:n_probe
                  
                    % wait before doing next probe
                    WaitSecs(.25);
                    
                    % which rectangle are we probing
                    probe_tmp = probe_rects_shuffled(adj_phase);

                    % ugly way to figure out current rectangle probe area
                    if probe_tmp==1 
                        rect_area_tmp = h1_tmp*w1_tmp;
                    else if probe_tmp==2
                        rect_area_tmp = h2_tmp*w2_tmp;
                    else if probe_tmp==3
                        rect_area_tmp = h3_tmp*w3_tmp;
                    end 

                    % figure out arrow location
                    probe_arrow_all_x= [r1_txt_x r2_txt_x r3_txt_x];
                    probe_arrow_x =probe_arrow_all_x(probe_tmp);
                    probe_arrow_head = [probe_arrow_x probe_arrow_y];
                    arrow_points = [
                        probe_arrow_head-[arrow_width,0]
                        probe_arrow_head+[arrow_width,0]
                        probe_arrow_head+[0,arrow_width]
                    ];
    
                    % get starting circle area
                    circle_area_tmp = unifrnd(mean_circle_area-max_circle_area_noise, 
                                              mean_circle_area+max_circle_area_noise
                    );
                    
                    % get starting circle radius
                    circle_rad_tmp = rad_from_area(circle_area_tmp);
    
                    % get circle loc
                    circle_x_ctr_tmp = unifrnd(circle_x_loc-max_circle_noise,
                                               circle_x_loc+max_circle_noise);
                    circle_y_ctr_tmp = unifrnd(circle_y_loc-max_circle_noise,
                                               circle_y_loc+max_circle_noise);    
                    % adjusting
                    adjusting = true;
    
                    % init RT variable
                    rt_total=0;

                    % init adjustment (zero)
                    adj=0;
                
                    % clear
%                    Screen('FillRect', screen_ptr, white);
                    % ADJUSTMENT PHASE
                    while adjusting
                        
                        % new circle radius
                        circle_rad_tmp = circle_rad_tmp+.5*adj;

                        % new circle area
                        circle_area_tmp = area_from_rad(circle_rad_tmp);

                        % make sure circle doesn't get smaller or larger than particular values
                        if circle_area_tmp > max_circle_area
                            circle_area_tmp = max_circle_area;
%                            disp(circle_area_tmp);
                            circle_rad_tmp = max_circle_rad;
%                            disp(circle_area_tmp);
    
                        else if circle_area_tmp < min_circle_area
                            circle_area_tmp = min_circle_area;
                            circle_rad_tmp = min_circle_rad;
                        end

                        % "square" where we draw the circle
                        circle_sq_tmp = [
                            circle_x_ctr_tmp - circle_rad_tmp
                            circle_y_ctr_tmp - circle_rad_tmp
                            circle_x_ctr_tmp + circle_rad_tmp
                            circle_y_ctr_tmp + circle_rad_tmp
                        ];
            
                        % draw outer rectangles
                        if h1_tmp > 0
                          Screen('FrameRect', screen_ptr, r1_border, r1_outer, rect_thick);
                          Screen('FillRect', screen_ptr, r1_fill, r1_inner);
%                          RDCL_DrawText(x=r1_txt_x, y=r1_txt_y, '1', 'Col',r1_border);
                        end
                        
                        if h2_tmp > 0
                          Screen('FrameRect', screen_ptr, r2_border, r2_outer, rect_thick);
                          Screen('FillRect', screen_ptr, r2_fill, r2_inner);
%                          RDCL_DrawText(x=r2_txt_x, y=r2_txt_y, '2', 'Col',r2_border);
                        end
                        
                        
                        if h3_tmp > 0
                          disp(r3_outer);
                          disp(r3_inner);
                          disp(r3_fill);
                          disp(r3_border);
                          disp(rect_thick);
                          disp(screen_ptr);
                          Screen('FrameRect', screen_ptr, r3_border, r3_outer, rect_thick);
                          Screen('FillRect', screen_ptr, r3_fill, r3_inner);
%                          RDCL_DrawText(x=r2_txt_x, y=r2_txt_y, '2', 'Col',r2_border);
                        end
                        
 
                        % draw arrow
                        Screen('FillPoly',screen_ptr,black,arrow_points);

                        % DRAW CIRCLE
                        Screen('FillOval', screen_ptr , circle_fill, circle_sq_tmp);
    
                        % SHOW EVERYTHING
                        [probe_onset_time_stamp] = Screen('Flip', screen_ptr);
    
                        % Get response 
                        [resp_key resp_num time_secs] = RDCL_GetResponse({
                            {'LeftArrow',adj_left}, 
                            {'RightArrow',adj_right},
                            {'UpArrow',adj_up},
                            {'DownArrow',adj_down},
                            {'f',0},
                            {'q',1111},
                        });
    
                        % Get RTs
                        % rt for current adjustment
                        % figure out difference between respoinse and stimulus onset, subtract previous rt total
                        rt_current = time_secs - probe_onset_time_stamp-rt_total;
    
                        % rt for total adjustment duration
                        rt_total = time_secs - probe_onset_time_stamp;
                        
                        % figure out response
                        switch resp_key
                            case 'LeftArrow'
                                adj=adj_left;
                            case 'RightArrow'
                                adj=adj_right;
                            case 'UpArrow'
                                adj=adj_up;
                            case 'DownArrow'
                                adj=adj_down;
                            case 'f'
                                adj=0;
                            case 'q'
                                adjusting=false;
                                sca;
                        end 
                                                                     
                        
                        
                         % FIGURE OUT IF THIS THE LAST ADJUSTMETNT
                        if strcmp(resp_key,'f')
                            adjusting = false;
                            points_tmp = compute_points(circle_area_tmp, rect_area_tmp);
                            total_points = total_points+round(points_tmp);
                            % WRITE OUTPUT TO FILE
                            fprintf(fp, '%s, %s, %s, %s, %d, %d, %d, %d, %d, %d, %d, %d, %d, %d, %d, %d,%d, %s, %d,  %d, %d, %d, %d, %d, %d \n',
                            sub_n, disp_cond, effect_tmp, set_tmp, distance_tmp, diag_tmp, block_num, trial_index, probe_tmp, h1, w1, h2, w2, h3, w3, circle_area_tmp, circle_rad_tmp, resp_key, resp_num,  adj, points_tmp, total_points, time_secs, rt_current, rt_total);
                            fflush(fp); 
                        else
                            % WRITE OUTPUT TO FILE
                            fprintf(fp, '%s, %s, %s, %s, %d, %d, %d, %d, %d, %d, %d, %d, %d, %d, %d, %d,%d, %s, %d,  %d, %d, %d, %d, %d, %d \n',
                            sub_n, disp_cond, effect_tmp, set_tmp, distance_tmp, diag_tmp, block_num, trial_index, probe_tmp, h1, w1, h2, w2, h3, w3, circle_area_tmp, circle_rad_tmp, resp_key, resp_num,  adj, NA, total_points, time_secs, rt_current, rt_total);
                            fflush(fp); 
                        endif   
                    end
                end
              end 
          end
      end
    end
    sca;
end
  % Get the image information for sizing information
%    debrief_info = imfinfo('Judg_choice_debrief.png');
%    debrief_width = debrief_info.Width;
%    debrief_height = debrief_info.Height;
%
%    % Get the image of debriefing instructions
%    debrief = imread('Judg_choice_debrief.png');
%
%    % Where and how big should the image be?
%    debrief_scale = .55;
%    debrief_rect = [S.middle_x - round(debrief_scale*debrief_width/1.5) 
%                S.middle_y - round(debrief_scale*debrief_height/1.5) 
%                S.middle_x + round(debrief_scale*debrief_width/1.5) 
%                S.middle_y + round(debrief_scale*debrief_height/1.5)];
%
%    % Put the image into the buffer
%    Screen('PutImage', screen_ptr, debrief, debrief_rect);
%    
%    %Display debriefing instructions. Hit space to end experiment
%    Screen('Flip', screen_ptr);
%    RDCL_GetResponse({{'space', 0}});

                       
    
      
            
   %          
%        rect_area_tmp = unifrnd(min_circle_area,max_circle_area)
%        h_tmp = round(rect_area_tmp*unifrnd(0,1));
%        w_tmp = rect_area_tmp/h_tmp;
%        effect_tmp = "calibration";
%        set_tmp = "calibration";
%        h1_tmp = h_tmp;
%        w1_tmp = w_tmp;
%        h2_tmp = 0;
%        w2_tmp = 0;
%        h3_tmp = 0;
%        w3_tmp = 0;
%        diag_tmp = 999;
%        distance_tmp = 0;
%        trial_index=0;
%        probe_rect=1;
%
%        rect_tmp_outer = CenterRectOnPointd([0 0 w_tmp h_tmp], rectangle_box_w*.5, rectangle_box_h*.5);
%        rect_tmp_inner = CenterRectOnPointd([0 0 w_tmp-rect_thick h_tmp-rect_thick], rectangle_box_w*.5, rectangle_box_h*.5);
%        Screen('FrameRect', screen_ptr, black, rect_tmp_outer , rect_thick);
%        Screen('FillRect', screen_ptr, rect_fill, rect_tmp_inner);
%
%        % get starting circle area
%        circle_area_tmp = unifrnd(mean_circle_area-max_circle_area_noise, 
%                                  mean_circle_area+max_circle_area_noise);
%        
%        % get starting circle radius
%        circle_rad_tmp = rad_from_area(circle_area_tmp);
%    
%    
%        % get circle loc
%        circle_x_ctr_tmp = unifrnd(circle_x_loc-max_circle_noise,
%                                   circle_x_loc+max_circle_noise);
%        circle_y_ctr_tmp = unifrnd(circle_y_loc-max_circle_noise,
%                                   circle_y_loc+max_circle_noise);    
%        % adjusting
%        adjusting = true;
%
%        % init RT variable
%        rt_total=0;
%
%        % init adjustment (zero)
%        adj=0;
%    
%        % clear
%        Screen('FillRect', screen_ptr, white);
%
%        % ADJUSTMENT PHASE
%        while adjusting
%            
%            % new circle radius
%            circle_rad_tmp = circle_rad_tmp+.5*adj;
%
%            % new circle area
%            circle_area_tmp = area_from_rad(circle_rad_tmp);
%
%            % make sure circle doesn't get smaller or larger than particular values
%            if circle_area_tmp > max_circle_area
%                circle_area_tmp = max_circle_area;
%                disp(circle_area_tmp);
%                circle_rad_tmp = max_circle_rad;
%                disp(circle_area_tmp);
%
%            else if circle_area_tmp < min_circle_area
%                circle_area_tmp = min_circle_area;
%                circle_rad_tmp = min_circle_rad;
%            end
%
%            % "square" where we draw the circle
%            circle_sq_tmp = [
%                circle_x_ctr_tmp - circle_rad_tmp
%                circle_y_ctr_tmp - circle_rad_tmp
%                circle_x_ctr_tmp + circle_rad_tmp
%                circle_y_ctr_tmp + circle_rad_tmp
%            ];
%
%            % draw arrow
%            %Screen('FillPoly',screen_ptr,black,arrow_points);
%
%            % DRAW CIRCLE
%            Screen('FillOval', screen_ptr , circle_fill, circle_sq_tmp);
%
%            % SHOW EVERYTHING
%            [probe_onset_time_stamp] = Screen('Flip', screen_ptr);
%
%            % Get response 
%            [resp_key resp_num time_secs] = RDCL_GetResponse({
%                {'LeftArrow',adj_left}, 
%                {'RightArrow',adj_right},
%                {'UpArrow',adj_up},
%                {'DownArrow',adj_down},
%                {'f',0},
%                {'q',1111},
%            });
%
%            % Get RTs
%            % rt for current adjustment
%            % figure out difference between respoinse and stimulus onset, subtract previous rt total
%            rt_current = time_secs - probe_onset_time_stamp-rt_total;
%
%            % rt for total adjustment duration
%            rt_total = time_secs - probe_onset_time_stamp;
%            
%            % figure out response
%            switch resp_key
%                case 'LeftArrow'
%                    adj=adj_left;
%                case 'RightArrow'
%                    adj=adj_right;
%                case 'UpArrow'
%                    adj=adj_up;
%                case 'DownArrow'
%                    adj=adj_down;
%                case 'f'
%                    adj=0;
%                case 'q'
%                    adjusting=false;
%                    sca;
%            end 
%
%            % new circle radius
%            circle_rad_tmp = circle_rad_tmp+.5*adj;
%
%            % new circle area
%            circle_area_tmp = area_from_rad(circle_rad_tmp);
%                                                        
%            % FIGURE OUT IF THIS THE LAST ADJUSTMETNT
%            if strcmp(resp_key,'f')
%                adjusting = false;
%                points_tmp = compute_points(circle_area_tmp, rect_area_tmp);
%                total_points = total_points+round(points_tmp);
%            end  
%
%            % WRITE OUTPUT TO FILE
%            fprintf(fp, '%s, %s, %s, %s, %d, %d, %d, %d, %d, %d, %d, %d, %d, %d, %d, %d,%d, %s, %d,  %d, %d, %d, %d, %d, %d \n',
%            sub_n, disp_cond, effect_tmp, set_tmp, distance_tmp, diag_tmp, block_num, trial_index, probe_rect, h1, w1, h2, w2, h3, w3, circle_area_tmp, circle_rad_tmp, resp_key, resp_num,  adj, current_points, total_points, time_secs, rt_current, rt_total);
%            fflush(fp); 
%        end 
%
%        WaitSecs(1);
%        Screen('FrameRect', screen_ptr, black, rect_tmp_outer , rect_thick);
%        Screen('FillRect', screen_ptr, rect_fill, rect_tmp_inner);
%        circle_rad_tmp = rad_from_area(h_tmp*w_tmp);
%        % "square" where we draw the circle
%        circle_sq_tmp = [
%            circle_x_ctr_tmp - circle_rad_tmp
%            circle_y_ctr_tmp - circle_rad_tmp
%            circle_x_ctr_tmp + circle_rad_tmp
%            circle_y_ctr_tmp + circle_rad_tmp
%        ];
%        % DRAW CIRCLE
%        Screen('FillOval', screen_ptr , circle_fill, circle_sq_tmp);
%
%        RDCL_DrawText(s_rect(2)*.5, s_rect(4)*.25, "Total Points:");
%        RDCL_DrawText(s_rect(2)*.5, s_rect(4)*.35, num2str(total_points));
%        Screen("Flip");
%        WaitSecs(2);
