% Fall 2023
% Circle-adjustment CE exp
function circle_area_exp
   
      more off;
      
      global screen_ptr;

      % CONSTANT VARIABLES %

      % NUMBER OF BLOCKS
      n_blocks = 4;

      %% vertical jitter
      vjit = 25; 

      % rect gray
      rect_fill = [87 88 91];

      % for now circle is same color as rectangles
      circle_fill = rect_fill;

      % rect thickness
      rect_thick = 1;
      
      % circle size (avg)
      mean_circle_area = 31284;

      % max circle location noise
      max_circle_noise = 100;

      % max circle area noise
      max_circle_area_noise = 2784;

      % arrow width
      arrow_width = 40;

      % little adjustments
      adj_left = -100;
      adj_right = 100;

      % big adjustments
      adj_down = -250
      adj_up = 250;
      
      % rectangle distance
      rect_dist = .12;
           
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
        output_name = ['circle_area_exp_' sub_n '.csv'];
      end 

      % get condition (odd subject # means trueblood style, even subject # means spektor style)
      if rem(str2double(sub_n),2)>0
        disp_cond = "horizontal";
      else
        disp_cond = "triangle";
      end 
      
      disp(disp_cond);

      % Create/open output file
      fp = fopen(fullfile(destination_folder,output_name),'w');     

      % Write to output file
      fprintf(fp, 'sub_n, disp_cond, type, set, opts, block_num, trial_index, probe_rect, h1, w1, h2, w2, h3, w3, circle_area, circle_rad, resp_key, resp_num,  adj, time_secs, rt_current, rt_total \n');

%%%%%% MORE SETUP %%%%%%%
      %Add path to RDCL_Functions
      addpath('./RDCL_Functions');    
      
      %Add path to utility functions
      addpath('./utility_functions');
      
      % add path to trial params
      addpath('./trial_params');
      
      % Dummy calls    
      RDCL_DummyCalls(); 
      
      % seed rng
      RDCL_SeedRandomNumbers();
 
%%%%%% INSTRUCTIONS %%%%%%%  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%   

      % Open Screen  
      [screen_ptr s_rect] = Screen('OpenWindow', 0, 0, [], 32, 2);
      disp(s_rect);
      disp('screen_ptr');
      disp(screen_ptr);
      % white
      white = [255 255 255];

      % black
      black = [0 0 0];

      % get location for circle center
      % This location remains the same for 1, 2, & 3 option trials, but will be jittered on each trial
      circle_x_loc = round(.75*s_rect(3));
      circle_y_loc = round(s_rect(4)*.25);

      % rectangle box center
%      rect_box_c_x = s_rect(3)*.25;
%      rect_box_c_y = s_rect(4)*.75;
%      rect_box_c = [rect_box_c_x rect_box_c_y];
%      
      % "box" where we draw the rectangles
      rectangle_box = [0 round(s_rect(4))*.5 round(s_rect(3)*.5) round(s_rect(4))]; 
 
      % width of the box
      rectangle_box_w = rectangle_box(3)-rectangle_box(1);
      rectangle_box_h = rectangle_box(4)-rectangle_box(2);
      rectangle_box_c = [ (rectangle_box(3)+rectangle_box(1))/2 rectangle_box(4)-.5*rectangle_box(2)];
      disp('rect box'); disp(rectangle_box); disp('rect box w'); disp(rectangle_box_w);disp('rect box h'); disp(rectangle_box_h);disp('rect box c'); disp(rectangle_box_c);
      % y location of probe arrow
      probe_arrow_y = s_rect(4)*.10;
 
      % Rectangles for testing
%      h1 = 200;
%      w1 = 150;
%      h2 = 150;
%      w2 = 200;
%      h3 = 125;
%      w3 = 200;
%      sets="test";
%      opts="test";
%      type="test";
       h1 = read_params("h1.txt","%d");
       w1 = read_params("w1.txt","%d");
       h2 = read_params("h2.txt","%d");
       w2 = read_params("w2.txt","%d");
       h3 = read_params("h3.txt","%d");
       w3 = read_params("w3.txt","%d");
       sets = read_params("set.txt","%s");
%       opts = read_params("opts.txt","%s");
%       type = read_params("type.txt","%s");
      % CHANGE THESE!! %
%      n_trials = [1]; 
      n_trials = numel(h1);

      % Shuffle trials
      trial_order = RDCL_RandomizeTrials(n_trials);

      for block_num = 1:n_blocks
          
          % loop through trials
          for trial_index = 1:n_trials 
    
            % Get shuffled trial number
            trial_num = trial_order(trial_index);

            % get set
            set_tmp = sets(trial_num);
            type_tmp = 'testing';%type(trial_num);
            opts_tmp = 'testing';%opts(trial_num);
    
             % heights (unshuffled)
             h_all_tmp_unshuffled = [h1(trial_num) h2(trial_num) h3(trial_num)];
             w_all_tmp_unshuffled = [w1(trial_num) w2(trial_num) w3(trial_num)];
    
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
              disp(r1_y+h1_tmp);
              disp(r2_y+h2_tmp);
              disp(r3_y+h3_tmp);
              
              
              
              txt_dist_y = max(round(r1_y+h1_tmp), 
                             round(r2_y+h2_tmp), 
                             round(r3_y+h3_tmp))+.025*rectangle_box_h;
              
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
    
                % INIT SCREEN
%                Screen('FillRect', screen_ptr, white);
%    
%                % draw outer rectangles
%                Screen('FrameRect', screen_ptr, r1_border, r1_outer, rect_thick);
%                Screen('FrameRect', screen_ptr, r2_border, r2_outer, rect_thick);
%                Screen('FrameRect', screen_ptr, r3_border, r3_outer, rect_thick);
%    
%                % draw inner rectangles
%                Screen('FillRect', screen_ptr, r1_fill, r1_inner);
%                Screen('FillRect', screen_ptr, r2_fill, r2_inner);
%                Screen('FillRect', screen_ptr, r3_fill, r3_inner);
%
%                % draw text labels
%                RDCL_DrawText(x=r1_txt_x, y=r1_txt_y, '1', 'Col',r1_border);
%                RDCL_DrawText(x=r2_txt_x, y=r2_txt_y, '2', 'Col',r2_border);
%                RDCL_DrawText(x=r3_txt_x, y=r3_txt_y, '3', 'Col',r3_border);
%
%                % Brief waiting phase
%                WaitSecs(1);
%
%                % show rectangles
%                Screen('Flip', screen_ptr, [], 1)
                
                % loop through rect probes
                for adj_phase = 1:n_probe
                  
                    % wait before doing next probe
                    WaitSecs(.25);
                    
                    % which rectangle are we probing
                    probe_tmp = probe_rects_shuffled(adj_phase);
    
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
                    circle_area_tmp = add_noise(mean_circle_area, max_circle_area_noise);
    
                    % get circle loc
                    circle_x_ctr_tmp = add_noise(circle_x_loc, max_circle_noise);
                    circle_y_ctr_tmp = add_noise(circle_y_loc, max_circle_noise);
    
                    % adjusting
                    adjusting = true;
    
                    % init RT variable
                    rt_total=0;

                    % init adjustment (zero)
                    adj=0;
                    % clear
                    Screen('FillRect', screen_ptr, white);
                    % ADJUSTMENT PHASE
                    while adjusting
                        disp('initial');
                        disp(circle_area_tmp);
                        
                        circle_area_tmp = circle_area_tmp+adj;
                        
                        disp('adjusted');
                        disp(circle_area_tmp),

                        % get circle radius
                        circle_rad_tmp = rad_from_area(circle_area_tmp);
    
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
                          Screen('FrameRect', screen_ptr, r3_border, r3_outer, rect_thick);
                          Screen('FillRect', screen_ptr, r3_fill, r3_inner);
%                          RDCL_DrawText(x=r3_txt_x, y=r3_txt_y, '3', 'Col',r3_border);
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
                        % WRITE OUTPUT TO FILE
                        fprintf(fp, '%s, %s, %s, %s, %s, %d, %d, %d,%d, %d, %d, %d, %d, %d, %d, %d, %s, %d,  %d, %d, %d, %d \n',
                        sub_n, disp_cond, type_tmp, set_tmp, opts_tmp, block_num, trial_index, probe_tmp, h1_tmp, w1_tmp, h2_tmp, w2_tmp, h3_tmp, w3_tmp, circle_area_tmp, circle_rad_tmp, resp_key, resp_num,  adj, time_secs, rt_current, rt_total);
                        fflush(fp);                        
                        
                          % FIGURE OUT IF THIS THE LAST ADJUSTMETNT
                        if strcmp(resp_key,'f')
                            adjusting = false;
                        endif
                    end
                end
          end
      end
      sca;
end
            

             
 
