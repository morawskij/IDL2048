;+
; NAME:
; PLAY2048
;
; PURPOSE:
; Crop out square shaped regions of 12 images to create customized tiles for the game of 2048.
; Modify brightness and contrast of the images presented on the tiles.
; Play the game. 
;
; CATEGORY:
; Entertainment
;
; CALLING SEQUENCE:
; PLAY2048
;
; INPUTS:
; No input parameters
; During the application run, 12 images must be loaded by pressing on relevant buttons. 
; They are stored in memory as file names, meaning that if an image file of a given name changes during the run of the programme, any change to the relevant tile will automatically update it with
; the new image. 
;
; OUTPUTS:
; None, the result is a game which can be played over and over as long as the application is running (restart button)
;
; SIDE EFFECTS:
; Risk of addiction
;
; RESTRICTIONS:
; Input images must be RGB images in the JPG or PNG format. If an image that doesn't satisfy this requirement (which is clearly specified in the instructions shown in the application), the program
; might malfunction. 
;
; PROCEDURE:
; First, images must be loaded and optionally edited. Jumbping between the images to change the way they were edited is possible by clicking on the tile prieview widgets
; Once 12 images are loaded, it is possible to start the game. It can then be restarted at any time. Arrows indicate the moves made by the user
;
; MODIFICATION HISTORY:
; Own work, Jakub Morawski, November 2023
;-

; The first few functions here are related specifically to the game mode and don't have anything to do with image processing. I will provide brief one-sentence comments for those

; This function generates the basic tile shape (that can be overlayed on an image)

function base_tile, p
 b = 2^(p-2)
 f = 1.
 a_big = fltarr(3,2^p+2*b,2^p+2*b)
 a_big[*,*,*]=0.25*f
 a_big[*,b+2^(p-4):b+2^p-1-2^(p-4),b+2^(p-4):b+2^p-1-2^(p-4)]=f
 w = 2^(p-2)
 a_big[0,*,*]=smooth(reform(a_big[0,*,*]),w)
 a_big[1,*,*]=smooth(reform(a_big[1,*,*]),w)
 a_big[2,*,*]=smooth(reform(a_big[2,*,*]),w)
 a = a_big[*,b:b+2^p-1,b:b+2^p-1]
 return, a
END

; This function generated a drawing of an arrow pointing right, of a given size

function arrow_drawing, ar_sz
 ar = bytarr(ar_sz,ar_sz)
 for i=0,ar_sz/2-1 do begin
 	ar[i,ar_sz/2-1]=255
 end
  for i=ar_sz/2,ar_sz-1 do begin
 	ar[i,ar_sz/2-1]=255
 	ar[i,i-ar_sz/2]=255
 	ar[i,3*ar_sz/2-i-1]=255
 end
 return, ar
END

; This function returns grid coordinates of a field chosen randomly among available free fields 

function random_tile_location, free_fields, g
    s = total(free_fields)
    inv_l = 1./s
    r = randomu(Seed,1)
    b = fix(r/inv_l)
    if b eq g^2 then begin
    	b=g^2-1
    end
    i=0
    j = 0
    s2 = 0
    while s2 lt b+1 do begin
    	s2+=free_fields[i,j]
    	if s2 lt b+1 then begin
	    	if j lt g-1 then begin
		    	j+=1
		end else begin
			j=0
			i+=1
		end
	end
    end
    return, [i,j]     
END

; This function is related to the processing of an image. It takes an array and modifies it based on values b (brightness, constant shift) and c (contrast, a linear point to point transformation)
; Contrast is applied first and brightness afterwards, which seems to yield better results and I imagine it is done in the same way in commercial image processing applications

function image_processing, img, b, c
	img_c = 255 < (255*((img-127.5*c/100.)/(255*(1-c/100.))) > 0)
	img_b = 255 < (img_c+b > 0)
	return, img_b
END

; Again, the first few procedures are related to the game mode. This one here checks if the user lost the game. If so, a global variable "lost" is set to 1 

pro check_if_lost
  COMMON id, a, p, g, tres, marg, marg_tile_prev, fields, free_fields, win, prev_win, null, lost, vict, tiles, preview_h, preview_w, active_image, sliders, slider_zoom, slider_x, slider_y, slider_bright, slider_contrast, tile_brightening_step, explanation_text9, explanation_text10, explanation_text13, current_highest, arrow_drawn
 if total(free_fields) eq 0 then begin
 maybe_lost=1B
 for i=0, g-1 do begin
 	for j=0, g-1 do begin
 		if i gt 0 then begin
 			if fields[i-1,j] eq fields[i,j] then begin
 				maybe_lost=0B
 			end
 		end
 		if j gt 0 then begin
 			if fields[i,j-1] eq fields[i,j] then begin
 				maybe_lost=0B
 			end
 		end
 		if maybe_lost eq 0B then begin
 			i=g
 			j=g
 		end
 	end
 end
 lost=maybe_lost
 end
END
 		
; This procedure displays the tiles in an arrangement corresponding the current state of the game. If "new" is equal 1 (most situations), it adds a new tile in a randomly chosen free field
; If the user wins or loses the game after their last move, the game area is covered by a yellow / grey square and a message about victory / loss is displayed

pro show_tiles, new
  COMMON id, a, p, g, tres, marg, marg_tile_prev, fields, free_fields, win, prev_win, null, lost, vict, tiles, preview_h, preview_w, active_image, slider_zoom, slider_x, slider_y, slider_bright, slider_contrast, tile_brightening_step, explanation_text9, explanation_text10, explanation_text13, current_highest, arrow_drawn
  wset, win
  for i=0, g-1 do begin
  	for j=0, g-1 do begin
  		if fields[i,j] eq 0 then begin
  			tv, null, i*(2^p+marg),j*(2^p+marg), /true
  		end else begin
  		 	tv, tiles.(fields[i,j]-1).tile_image, i*(2^p+marg),j*(2^p+marg), /true	
  		end
  	end
  end  	 
  if new eq 1B then begin
  	 loc = random_tile_location(free_fields,g)
  	 x = loc[0]
  	 y = loc[1]
  	 r = randomu(Seed,1)
  	 free_fields[x,y]=0
  	 if r gt tres then begin
  	 	v=2
  	 end else begin
	     v=1
	 end
	 if v gt current_highest then begin
	 	current_highest = v-1
	 end
	 fields[x,y]=v
  	 tv, tiles.(fields[x,y]-1).tile_image, x*(2^p+marg),y*(2^p+marg), /true
  end
  if lost eq 2B then begin
  	wait, 2
  	overlay = bytarr(3,g*(2^p+marg), g*(2^p+marg))
  	overlay [0,*,*] = 252
   	overlay [1,*,*] = 215
   	overlay [2,*,*] = 95
  	tv, overlay, /true
  	widget_control, explanation_text9, set_value="Congratulations!"
   	widget_control, explanation_text10, set_value="You won!"
  end else begin
	  check_if_lost
	end
  if lost eq 1B then begin
  	wait, 2
  	overlay = bytarr(g*(2^p+marg), g*(2^p+marg))
  	overlay [*,*] = 96
  	tv, overlay
  	widget_control, explanation_text9, set_value="You lost"
   	widget_control, explanation_text10, set_value="Try again!"
  end  
END

; This function updates the grid of tiles in accordance with the move made by the player, and displays an arrow illustrating the move

pro update, t
  COMMON id, a, p, g, tres, marg, marg_tile_prev, fields, free_fields, win, prev_win, null, lost, vict, tiles, preview_h, preview_w, active_image, slider_zoom, slider_x, slider_y, slider_bright, slider_contrast, tile_brightening_step, explanation_text9, explanation_text10, explanation_text13, current_highest, arrow_drawn
 new_free=intarr(g,g)
 new_free[*]=1
 wset, prev_win
 if t eq 8 then begin
 	process_fields=fields 
 	tv, transpose(reverse(arrow_drawn)), preview_w*0.375, preview_h*0.375
 end else if t eq 7 then begin
  	process_fields=reverse(fields,2)
  	tv, transpose(arrow_drawn), preview_w*0.375, preview_h*0.375
 end else if t eq 6 then begin
  	process_fields=transpose(reverse(fields))
  	tv, arrow_drawn, preview_w*0.375, preview_h*0.375
 end else if t eq 5 then begin
  	process_fields=transpose(fields)
  	tv, reverse(arrow_drawn), preview_w*0.375, preview_h*0.375
 end
 wait, 0.1
 tv, 0*tiles.(0).preview_image, /true
 for i=0, g-1 do begin
     j=0
     sorted=2B
     new_row=intarr(g)
     k=0
     while j lt g do begin
       	if process_fields[i,j] eq 0 then begin
	    j+=1
	end else begin
	     st_j=j
	     j+=1
	     sorted=0B
	     f_j=g
	     while j lt g do begin
	     		if process_fields[i,j] eq 0 then begin
	      	  		j+=1
	      	  	end else begin
	      	  		sorted=1B
	      	  		if process_fields[i,j] eq process_fields[i,st_j] then begin
	      	  			new_row[k]=process_fields[i,j]+1
	      	  			if new_row[k] gt current_highest+1 then begin
	      	  				current_highest+=1
	      	  			end
	      	  			if new_row[k] eq vict then begin
	      	  				lost = 2B
	      	  			end
     					k+=1
     					f_j=j+1
     					j=g
     				end else begin
     					new_row[k]=process_fields[i,st_j]
     					k+=1
     					f_j=j
     					j=g
     				end
     			end
     		end
     		j=f_j
     	end
     end
     	if sorted eq 0B then begin
     		new_row[k]=process_fields[i,st_j]
     		k+=1
     	end

	if k gt 0 then begin
	     	process_fields[i,*]=new_row
     		new_free[i,0:k-1]=0
     		if k lt g then begin
	     		new_free[i,k:g-1]=1
	     	end
     	end
     
end
 if t eq 8 then begin
 	fields=process_fields
 	free_fields=new_free 
 end else if t eq 7 then begin
  	fields=reverse(process_fields,2)
  	free_fields=reverse(new_free,2)
 end else if t eq 6 then begin
  	fields=reverse(transpose(process_fields))
  	free_fields=reverse(transpose(new_free))
 end else if t eq 5 then begin
  	fields=transpose(process_fields)
     	free_fields=transpose(new_free)
 end
END		

; Remaining procedures relate to the first part of the application's functionality - loading and processing 12 images

; This procedure displays the preview of the "n"th tile, with a blue border around if it's active (meaning currently being edited in the edit mode or highest reached by the user in game mode)

PRO tile_display, n, is_active
  COMMON id, a, p, g, tres, marg, marg_tile_prev, fields, free_fields, win, prev_win, null, lost, vict, tiles, preview_h, preview_w, active_image, slider_zoom, slider_x, slider_y, slider_bright, slider_contrast, tile_brightening_step, explanation_text9, explanation_text10, explanation_text13, current_highest, arrow_drawn
  	to_display = fltarr(3,2^p+2*marg_tile_prev,2^p+2*marg_tile_prev) 
        to_display[*,*,*]=0
        if is_active eq 1B then begin
        	to_display[2,*,*]=255
        end 
  	to_display[*,marg_tile_prev:2^p+marg_tile_prev-1,marg_tile_prev:2^p+marg_tile_prev-1]=tiles.(n).tile_image
  	wset, tiles.(n).tile_preview_window
  	tv, to_display, /true
END

; This procedure displays the image which is currently being edited in three ways:
; 1) As a whole, with a white square marking the area cropped for the tile
; 2) A preview of the cropped area
; 3) A small preview of the tile (which is different as it has an overlay of a progressively brighter base tile) 
     
PRO display_image, n
  COMMON id, a, p, g, tres, marg, marg_tile_prev, fields, free_fields, win, prev_win, null, lost, vict, tiles, preview_h, preview_w, active_image, slider_zoom, slider_x, slider_y, slider_bright, slider_contrast, tile_brightening_step, explanation_text9, explanation_text10, explanation_text13, current_highest, arrow_drawn
	tiles.(n).preview_image[*,*,*]=0  
	if strmatch(tiles.(n).filename, "*.jp*g") then begin 
	  	read_jpeg, tiles.(n).filename, img0
	end else if strmatch(tiles.(n).filename,"*.png") then begin
		read_png, tiles.(n).filename, img0
	end
	
  	img0 = image_processing(img0,tiles.(n).b_val,tiles.(n).c_val)
  		
  	sz = size(img0, /dim)
  	
  	if size(sz,/n_elements) lt 3 then begin
  		img = bytarr(3,sz[0],sz[1])
  		img[0,*,*]=reform(img0,[1,sz[0],sz[1]])
  		img[1,*,*]=reform(img0,[1,sz[0],sz[1]])
  		img[2,*,*]=reform(img0,[1,sz[0],sz[1]])
  	end else begin
  		img=img0
  	end
  	
  	sz = size(img, /dim)

        scl = sz[2]/float(preview_h)
        sz1_scaled = sz[1]/scl
        if sz1_scaled gt preview_w then begin
        	scl = sz[1]/float(preview_w)
        	sz2_scaled = sz[2]/scl
        	x0 = 0
        	y0 = fix((preview_h-sz2_scaled)/2.)
        	x1 = preview_w-1
        	y1 = y0+sz2_scaled-1
        	tiles.(n).preview_image[*,*,y0:y1] = congrid(img,3,preview_w,sz2_scaled)

        end else begin
        	y0 = 0
        	x0 = fix((preview_w-sz1_scaled)/2.)
        	y1 = preview_h-1
        	x1 = x0+sz1_scaled-1
	        tiles.(n).preview_image[*,x0:x1,*] = congrid(img,3,sz1_scaled,preview_h)
	end
	sq = x1-x0 < y1-y0
	sq = sq/tiles.(n).z_val
        wset, prev_win
        tv, tiles.(n).preview_image, /true    
        x0true = x0+fix(tiles.(n).x_val*(x1-x0-sq))
        y0true = y0+fix(tiles.(n).y_val*(y1-y0-sq))
  	plots, [x0true, x0true+sq, x0true+sq, x0true,x0true], [y0true, y0true, y0true+sq, y0true+sq,y0true], /device
  	tiles.(n).cropped_image[*,*,*]=congrid(img[*,scl*(x0true-x0):scl*(x0true-x0+sq),scl*(y0true-y0):scl*(y0true-y0+sq)],3,g*(2^p+marg),g*(2^p+marg))
  	wset, win
  	tv, tiles.(n).cropped_image, /true
	tiles.(n).tile_image[*,*,*]=(congrid(tiles.(n).cropped_image,3,2^p,2^p)*(1.-(n*tile_brightening_step)/255.)+n*tile_brightening_step)*a
	tile_display, n, 1B
END  	
     	
; This procedure activates an image. In a typical case of "display" equal to 1 it means displaying the "n"th image, otherwise only the "n"th tile preview gets a blue border
     	
PRO activate_image, n, display
  COMMON id, a, p, g, tres, marg, marg_tile_prev, fields, free_fields, win, prev_win, null, lost, vict, tiles, preview_h, preview_w, active_image, slider_zoom, slider_x, slider_y, slider_bright, slider_contrast, tile_brightening_step, explanation_text9, explanation_text10, explanation_text13, current_highest, arrow_drawn
      active_image = n
      widget_control, slider_zoom, set_value=fix(tiles.(n).z_val*100)
      widget_control, slider_x, set_value=fix(tiles.(n).x_val*100)
      widget_control, slider_y, set_value=fix(tiles.(n).y_val*100)
      widget_control, slider_bright, set_value=tiles.(n).b_val
      widget_control, slider_contrast, set_value=tiles.(n).c_val
        if display eq 1B then begin
	      display_image, n
	end
	for i=0,11 do begin
		if i eq n then begin
			tile_display, i, 1B
		end else begin
			tile_display, i, 0B
		end
	end
END

; This procedure is activated by pressing the buttons for loading images. It let's the user pick an image from the computer, loads it with default settings for edition and activates it.

PRO load_image, n, str
  COMMON id, a, p, g, tres, marg, marg_tile_prev, fields, free_fields, win, prev_win, null, lost, vict, tiles, preview_h, preview_w, active_image, slider_zoom, slider_x, slider_y, slider_bright, slider_contrast, tile_brightening_step, explanation_text9, explanation_text10, explanation_text13, current_highest, arrow_drawn
      file = pickfile()
      tiles.(n).filename = file
      widget_control, tiles.(n).load_info, set_value=str
      widget_control, tiles.(n).choose_file, set_value="Change image"
      tiles.(n).x_val = 0.5
      tiles.(n).y_val = 0.5
      tiles.(n).z_val = 1.
      tiles.(n).b_val = 0.
      tiles.(n).c_val = 0.
      activate_image, n, 1B
      tiles.(n).is_loaded = 1B
END

; The main event procedure:
; Loading images with load buttons
; Activating a given image for edition by pressing the tile preview
; Sliders changing the edition settings
; Starting the game with the start button if all 12 images have been loaded
; Gameplay executed by pressing arrow keys

PRO play2048_event, s
  COMMON id, a, p, g, tres, marg, marg_tile_prev, fields, free_fields, win, prev_win, null, lost, vict, tiles, preview_h, preview_w, active_image, slider_zoom, slider_x, slider_y, slider_bright, slider_contrast, tile_brightening_step, explanation_text9, explanation_text10, explanation_text13, current_highest, arrow_drawn

  widget_control, s.id, get_uvalue=u
  
  CASE u OF
  'pa': begin
     	 if s.press eq 1 and lost eq 0B then begin
   	   wset, win
	  	 t = s.key
	  	 free1 = free_fields
  		 update, t
  		 free2 = free_fields
  		 if total(free2) eq total(free1*free2) then begin
  		 	new = 0B
  		 end else begin
  		 	new = 1B
  		 end
       		 show_tiles, new
       	end
	activate_image, current_highest, 0B
  	end  
  
  	 
  'sg': begin
  	check = 0B
  	for i=0,11 do begin
  		check+=tiles.(i).is_loaded
  	end
  	if check lt 12 then begin
  		widget_control, explanation_text9, set_value="You have to load images for all 12 tiles before starting the game!!!"
   		widget_control, explanation_text10, set_value="Can't start!!!"
  	end else begin
  		current_highest = 0B
  		fields = intarr(g,g)
  		free_fields = intarr(g,g)
  		free_fields[*]=1
  		widget_control, explanation_text9, set_value="Game mode initiated successfully"
   		widget_control, explanation_text10, set_value="Enjoy!"
   		widget_control, s.id, set_value="Restart game"

   		lost = 0B
   		wset, prev_win
   		tv, 0*tiles.(0).preview_image, /true
   		wset, win
   		tv, 0*tiles.(0).cropped_image, /true
   		show_tiles, 1B
   		activate_image, current_highest, 0B
   		widget_control, explanation_text13, set_value="Press on the game area to start playing"
  	end
  end
  
"t0p": begin
if tiles.(0).is_loaded eq 1B and lost eq 3B then begin
activate_image, 0B, 1B
end
end
"t0f": begin
if lost eq 3B then begin
load_image, 0B, "Tile 1, image chosen"
end
end

"t1p": begin
if tiles.(1).is_loaded eq 1B and lost eq 3B then begin
activate_image, 1B, 1B
end
end
"t1f": begin
if lost eq 3B then begin
load_image, 1B, "Tile 2, image chosen"
end
end

"t2p": begin
if tiles.(2).is_loaded eq 1B and lost eq 3B then begin
activate_image, 2B, 1B
end
end
"t2f": begin
if lost eq 3B then begin
load_image, 2B, "Tile 4, image chosen"
end
end

"t3p": begin
if tiles.(3).is_loaded eq 1B and lost eq 3B then begin
activate_image, 3B, 1B
end
end
"t3f": begin
if lost eq 3B then begin
load_image, 3B, "Tile 8, image chosen"
end
end

"t4p": begin
if tiles.(4).is_loaded eq 1B and lost eq 3B then begin
activate_image, 4B, 1B
end
end
"t4f": begin
if lost eq 3B then begin
load_image, 4B, "Tile 16, image chosen"
end
end

"t5p": begin
if tiles.(5).is_loaded eq 1B and lost eq 3B then begin
activate_image, 5B, 1B
end
end
"t5f": begin
if lost eq 3B then begin
load_image, 5B, "Tile 32, image chosen"
end
end

"t6p": begin
if tiles.(6).is_loaded eq 1B and lost eq 3B then begin
activate_image, 6B, 1B
end
end
"t6f": begin
if lost eq 3B then begin
load_image, 6B, "Tile 64, image chosen"
end
end

"t7p": begin
if tiles.(7).is_loaded eq 1B and lost eq 3B then begin
activate_image, 7B, 1B
end
end
"t7f": begin
if lost eq 3B then begin
load_image, 7B, "Tile 128, image chosen"
end
end

"t8p": begin
if tiles.(8).is_loaded eq 1B and lost eq 3B then begin
activate_image, 8B, 1B
end
end
"t8f": begin
if lost eq 3B then begin
load_image, 8B, "Tile 256, image chosen"
end
end

"t9p": begin
if tiles.(9).is_loaded eq 1B and lost eq 3B then begin
activate_image, 9B, 1B
end
end
"t9f": begin
if lost eq 3B then begin
load_image, 9B, "Tile 512, image chosen"
end
end

"t10p": begin
if tiles.(10).is_loaded eq 1B and lost eq 3B then begin
activate_image, 10B, 1B
end
end
"t10f": begin
if lost eq 3B then begin
load_image, 10B, "Tile 1024, image chosen"
end
end

"t11p": begin
if tiles.(11).is_loaded eq 1B and lost eq 3B then begin
activate_image, 11B, 1B
end
end
"t11f": begin
if lost eq 3B then begin
load_image, 11B, "Tile 2048, image chosen"
end
end

  	
     
         'sz': begin
               if lost eq 3B then begin
               widget_control, s.id, GET_VALUE=z
               tiles.(active_image).z_val = z/100.
               display_image, active_image
               end
         end
         'sx': begin
         	if lost eq 3B then begin
               widget_control, s.id, GET_VALUE=x
               tiles.(active_image).x_val = x/100.
               display_image, active_image
               end
         end
         'sy': begin
         if lost eq 3B then begin
               widget_control, s.id, GET_VALUE=y
               tiles.(active_image).y_val = y/100.
               display_image, active_image
               end
         end
         'sb': begin
         if lost eq 3B then begin
               widget_control, s.id, GET_VALUE=b
               tiles.(active_image).b_val = b
               display_image, active_image
               end
         end
         'sc': begin
         if lost eq 3B then begin
               widget_control, s.id, GET_VALUE=c
               tiles.(active_image).c_val = c
               display_image, active_image
               end
         end
  ENDCASE

END

; Definition of all widgets, setup of the application window and data structure tiles needed for other procedures

PRO play2048, ptr
  COMMON id, a, p, g, tres, marg, marg_tile_prev, fields, free_fields, win, prev_win, null, lost, vict, tiles, preview_h, preview_w, active_image, slider_zoom, slider_x, slider_y, slider_bright, slider_contrast, tile_brightening_step, explanation_text9, explanation_text10, explanation_text13, current_highest, arrow_drawn
  g = 4
  marg=3
  marg_tile_prev=4
  scrsz   = get_screen_size()
  p=5
  while g*(2^(p+1)+marg) lt 0.65*scrsz[1] do begin
  	p+=1
  end
  while g*(2^p+marg)+834+2*(2^p+2*marg_tile_prev) gt scrsz[0] do begin
  	p-=1
  end
  tile_brightening_step=5B
  tres=0.9
  a = base_tile(p)
  lost = 3B
  vict=11
  preview_h = scrsz[1]-g*(2^p+marg)-130
  preview_w = (g)*(2^p+marg)
  arrow_drawn = arrow_drawing(preview_h/4)
  null = intarr(3,2^p,2^p)
  null[*,*,*] = 64
  active_image = 0B
  base = widget_base(title= 'Play 2048',xsize=scrsz[0],ysize=scrsz[1],/row)
  pad = (scrsz[0]-(g*(2^p+marg)+834+2*(2^p+2*marg_tile_prev)))*0.1
  padh1 = widget_base(base,xsize=2*pad)
  info_buttons_sliders = widget_base(base,/column)
  padv5 = widget_base(info_buttons_sliders,ysize=30)
  welcome_field = widget_base(info_buttons_sliders,/column)
  welcome_text = widget_label(welcome_field,value="Welcome",font="Helvetica")
  explanation_text0 = widget_label(welcome_field,value="",/align_left)
  explanation_text1 = widget_label(welcome_field,value="This application allows you to edit 12 images that will become tiles for your customized version of the",/align_left)
  explanation_text2 = widget_label(welcome_field,value="classic 2048 game. For esthetic reasons (to be able to display the tiles in two columns and buttons in",/align_left)
  explanation_text3 = widget_label(welcome_field,value="three), but also to make it more challenging, the starting tile corresponds to 1, and not to 2, so",/align_left)
  explanation_text4 = widget_label(welcome_field,value="a victory of reaching the tile 2048 is equivalent to continuing the original game up to 4096.",/align_left)
  explanation_text5 = widget_label(welcome_field,value="",/align_left)
  explanation_text6 = widget_label(welcome_field,value="You can load the images using the buttons below and edit resulting tiles using the sliders. The images",/align_left)
  explanation_text7 = widget_label(welcome_field,value="must be JPG or PNG files. By pressing on a previously modified tile, you can resume editing it.",/align_left)
  explanation_text8 = widget_label(welcome_field,value="",/align_left)
  explanation_text9 = widget_label(welcome_field,value="Once you upload all the images and edit them to your satisfaction, press the button below to enter",/align_left)
  explanation_text10 = widget_label(welcome_field,value="game mode:",/align_left)
  explanation_text11 = widget_label(welcome_field,value="",/align_left)
  start_game = widget_button(welcome_field, uvalue="sg",value="Start game")
  explanation_text12 = widget_label(welcome_field,value="",/align_left)
  explanation_text13 = widget_label(welcome_field,value="After entering game mode you need to press on the game area to start interacting with this window.",/align_left)
  padv2 = widget_base(info_buttons_sliders,ysize=30)
  loading = widget_base(info_buttons_sliders,/row)
  loading1 = widget_base(loading,xsize=210,/column)
  loading2 = widget_base(loading,xsize=210,/column)
  loading3 = widget_base(loading,xsize=210,/column)
  padv1 = widget_base(info_buttons_sliders,ysize=50)
  sliders = widget_base(info_buttons_sliders,xsize=634,/column)
  slider_zoom_title = widget_label(sliders,value="Zoom level (%)")
  slider_zoom = widget_slider(sliders, uvalue='sz', value=100,  mini=100, maxi=500,/drag)
  slider_x_title = widget_label(sliders,value="x position (%)")
  slider_x = widget_slider(sliders, uvalue='sx', value=50,  mini=0, maxi=100, /drag)
  slider_y_title = widget_label(sliders,value="y position (%)")
  slider_y = widget_slider(sliders, uvalue='sy', value=50,  mini=0, maxi=100, /drag)
  slider_bright_title = widget_label(sliders,value="Brightness")
  slider_bright = widget_slider(sliders, uvalue='sb', value=0,  mini=-100, maxi=100, /drag)
  slider_contrast_title = widget_label(sliders,value="Contrast")
  slider_contrast = widget_slider(sliders, uvalue='sc', value=0,  mini=-100, maxi=100, /drag)
  padh2 = widget_base(base,xsize=pad)
  windows = widget_base(base,/column)
  padv6 = widget_base(windows,ysize=30)
  play_area = widget_draw(windows, uvalue='pa',xsize=g*(2^p+marg), ysize=g*(2^p+marg), keyboard_events=2)
  padv7 = widget_base(windows,ysize=20)
  image_preview = widget_draw(windows, uvalue='imp',xsize=preview_w,ysize=preview_h)
  padh3 = widget_base(base,xsize=pad)
  tiles_preview = widget_base(base,/column)
  padv3 = widget_base(tiles_preview,ysize=30)
  tiles_preview_title = widget_label(tiles_preview, value="Your collection of tiles",font="Helvetica",/align_center)
  padv4 = widget_base(tiles_preview,ysize=20)
  tiles_preview_sub = widget_base(tiles_preview,/row)
  tiles_preview1 = widget_base(tiles_preview_sub,/column)
  tiles_preview2 = widget_base(tiles_preview_sub,/column)
  
tile0_load_field = widget_base(loading1,/column)
tile0_filename = ""
tile0_preview_image = bytarr(3,preview_w,preview_h)
tile0_cropped_image = bytarr(3,g*(2^p+marg),g*(2^p+marg))
tile0_tile_image = a*0B*tile_brightening_step
tile0_load_info = widget_label(tile0_load_field,value="Choose image for tile 1", font="Helvetica", /align_center)
tile0_choose_file = widget_button(tile0_load_field,value="Choose file",uvalue="t0f", xsize = 100)
tile0_preview_area = widget_base(tiles_preview1,/row,/base_align_center)
tile0_desc = widget_label(tile0_preview_area, value="Tile 1",font="Helvetica",xsize=100,/align_center)
tile0_preview = widget_draw(tile0_preview_area,xsize=2^p+2*marg_tile_prev,ysize=2^p+2*marg_tile_prev,uvalue="t0p",/button_events)

tile1_load_field = widget_base(loading1,/column)
tile1_filename = ""
tile1_preview_image = bytarr(3,preview_w,preview_h)
tile1_cropped_image = bytarr(3,g*(2^p+marg),g*(2^p+marg))
tile1_tile_image = a*1B*tile_brightening_step
tile1_load_info = widget_label(tile1_load_field,value="Choose image for tile 2", font="Helvetica", /align_center)
tile1_choose_file = widget_button(tile1_load_field,value="Choose file",uvalue="t1f", xsize = 100)
tile1_preview_area = widget_base(tiles_preview1,/row,/base_align_center)
tile1_desc = widget_label(tile1_preview_area, value="Tile 2",font="Helvetica",xsize=100,/align_center)
tile1_preview = widget_draw(tile1_preview_area,xsize=2^p+2*marg_tile_prev,ysize=2^p+2*marg_tile_prev,uvalue="t1p",/button_events)

tile2_load_field = widget_base(loading1,/column)
tile2_filename = ""
tile2_preview_image = bytarr(3,preview_w,preview_h)
tile2_cropped_image = bytarr(3,g*(2^p+marg),g*(2^p+marg))
tile2_tile_image = a*2B*tile_brightening_step
tile2_load_info = widget_label(tile2_load_field,value="Choose image for tile 4", font="Helvetica", /align_center)
tile2_choose_file = widget_button(tile2_load_field,value="Choose file",uvalue="t2f", xsize = 100)
tile2_preview_area = widget_base(tiles_preview1,/row,/base_align_center)
tile2_desc = widget_label(tile2_preview_area, value="Tile 4",font="Helvetica",xsize=100,/align_center)
tile2_preview = widget_draw(tile2_preview_area,xsize=2^p+2*marg_tile_prev,ysize=2^p+2*marg_tile_prev,uvalue="t2p",/button_events)

tile3_load_field = widget_base(loading1,/column)
tile3_filename = ""
tile3_preview_image = bytarr(3,preview_w,preview_h)
tile3_cropped_image = bytarr(3,g*(2^p+marg),g*(2^p+marg))
tile3_tile_image = a*3B*tile_brightening_step
tile3_load_info = widget_label(tile3_load_field,value="Choose image for tile 8", font="Helvetica", /align_center)
tile3_choose_file = widget_button(tile3_load_field,value="Choose file",uvalue="t3f", xsize = 100)
tile3_preview_area = widget_base(tiles_preview1,/row,/base_align_center)
tile3_desc = widget_label(tile3_preview_area, value="Tile 8",font="Helvetica",xsize=100,/align_center)
tile3_preview = widget_draw(tile3_preview_area,xsize=2^p+2*marg_tile_prev,ysize=2^p+2*marg_tile_prev,uvalue="t3p",/button_events)

tile4_load_field = widget_base(loading2,/column)
tile4_filename = ""
tile4_preview_image = bytarr(3,preview_w,preview_h)
tile4_cropped_image = bytarr(3,g*(2^p+marg),g*(2^p+marg))
tile4_tile_image = a*4B*tile_brightening_step
tile4_load_info = widget_label(tile4_load_field,value="Choose image for tile 16", font="Helvetica", /align_center)
tile4_choose_file = widget_button(tile4_load_field,value="Choose file",uvalue="t4f", xsize = 100)
tile4_preview_area = widget_base(tiles_preview1,/row,/base_align_center)
tile4_desc = widget_label(tile4_preview_area, value="Tile 16",font="Helvetica",xsize=100,/align_center)
tile4_preview = widget_draw(tile4_preview_area,xsize=2^p+2*marg_tile_prev,ysize=2^p+2*marg_tile_prev,uvalue="t4p",/button_events)

tile5_load_field = widget_base(loading2,/column)
tile5_filename = ""
tile5_preview_image = bytarr(3,preview_w,preview_h)
tile5_cropped_image = bytarr(3,g*(2^p+marg),g*(2^p+marg))
tile5_tile_image = a*5B*tile_brightening_step
tile5_load_info = widget_label(tile5_load_field,value="Choose image for tile 32", font="Helvetica", /align_center)
tile5_choose_file = widget_button(tile5_load_field,value="Choose file",uvalue="t5f", xsize = 100)
tile5_preview_area = widget_base(tiles_preview1,/row,/base_align_center)
tile5_desc = widget_label(tile5_preview_area, value="Tile 32",font="Helvetica",xsize=100,/align_center)
tile5_preview = widget_draw(tile5_preview_area,xsize=2^p+2*marg_tile_prev,ysize=2^p+2*marg_tile_prev,uvalue="t5p",/button_events)

tile6_load_field = widget_base(loading2,/column)
tile6_filename = ""
tile6_preview_image = bytarr(3,preview_w,preview_h)
tile6_cropped_image = bytarr(3,g*(2^p+marg),g*(2^p+marg))
tile6_tile_image = a*6B*tile_brightening_step
tile6_load_info = widget_label(tile6_load_field,value="Choose image for tile 64", font="Helvetica", /align_center)
tile6_choose_file = widget_button(tile6_load_field,value="Choose file",uvalue="t6f", xsize = 100)
tile6_preview_area = widget_base(tiles_preview2,/row,/base_align_center)
tile6_desc = widget_label(tile6_preview_area, value="Tile 64",font="Helvetica",xsize=100,/align_center)
tile6_preview = widget_draw(tile6_preview_area,xsize=2^p+2*marg_tile_prev,ysize=2^p+2*marg_tile_prev,uvalue="t6p",/button_events)

tile7_load_field = widget_base(loading2,/column)
tile7_filename = ""
tile7_preview_image = bytarr(3,preview_w,preview_h)
tile7_cropped_image = bytarr(3,g*(2^p+marg),g*(2^p+marg))
tile7_tile_image = a*7B*tile_brightening_step
tile7_load_info = widget_label(tile7_load_field,value="Choose image for tile 128", font="Helvetica", /align_center)
tile7_choose_file = widget_button(tile7_load_field,value="Choose file",uvalue="t7f", xsize = 100)
tile7_preview_area = widget_base(tiles_preview2,/row,/base_align_center)
tile7_desc = widget_label(tile7_preview_area, value="Tile 128",font="Helvetica",xsize=100,/align_center)
tile7_preview = widget_draw(tile7_preview_area,xsize=2^p+2*marg_tile_prev,ysize=2^p+2*marg_tile_prev,uvalue="t7p",/button_events)

tile8_load_field = widget_base(loading3,/column)
tile8_filename = ""
tile8_preview_image = bytarr(3,preview_w,preview_h)
tile8_cropped_image = bytarr(3,g*(2^p+marg),g*(2^p+marg))
tile8_tile_image = a*8B*tile_brightening_step
tile8_load_info = widget_label(tile8_load_field,value="Choose image for tile 256", font="Helvetica", /align_center)
tile8_choose_file = widget_button(tile8_load_field,value="Choose file",uvalue="t8f", xsize = 100)
tile8_preview_area = widget_base(tiles_preview2,/row,/base_align_center)
tile8_desc = widget_label(tile8_preview_area, value="Tile 256",font="Helvetica",xsize=100,/align_center)
tile8_preview = widget_draw(tile8_preview_area,xsize=2^p+2*marg_tile_prev,ysize=2^p+2*marg_tile_prev,uvalue="t8p",/button_events)

tile9_load_field = widget_base(loading3,/column)
tile9_filename = ""
tile9_preview_image = bytarr(3,preview_w,preview_h)
tile9_cropped_image = bytarr(3,g*(2^p+marg),g*(2^p+marg))
tile9_tile_image = a*9B*tile_brightening_step
tile9_load_info = widget_label(tile9_load_field,value="Choose image for tile 512", font="Helvetica", /align_center)
tile9_choose_file = widget_button(tile9_load_field,value="Choose file",uvalue="t9f", xsize = 100)
tile9_preview_area = widget_base(tiles_preview2,/row,/base_align_center)
tile9_desc = widget_label(tile9_preview_area, value="Tile 512",font="Helvetica",xsize=100,/align_center)
tile9_preview = widget_draw(tile9_preview_area,xsize=2^p+2*marg_tile_prev,ysize=2^p+2*marg_tile_prev,uvalue="t9p",/button_events)

tile10_load_field = widget_base(loading3,/column)
tile10_filename = ""
tile10_preview_image = bytarr(3,preview_w,preview_h)
tile10_cropped_image = bytarr(3,g*(2^p+marg),g*(2^p+marg))
tile10_tile_image = a*10B*tile_brightening_step
tile10_load_info = widget_label(tile10_load_field,value="Choose image for tile 1024", font="Helvetica", /align_center)
tile10_choose_file = widget_button(tile10_load_field,value="Choose file",uvalue="t10f", xsize = 100)
tile10_preview_area = widget_base(tiles_preview2,/row,/base_align_center)
tile10_desc = widget_label(tile10_preview_area, value="Tile 1024",font="Helvetica",xsize=100,/align_center)
tile10_preview = widget_draw(tile10_preview_area,xsize=2^p+2*marg_tile_prev,ysize=2^p+2*marg_tile_prev,uvalue="t10p",/button_events)

tile11_load_field = widget_base(loading3,/column)
tile11_filename = ""
tile11_preview_image = bytarr(3,preview_w,preview_h)
tile11_cropped_image = bytarr(3,g*(2^p+marg),g*(2^p+marg))
tile11_tile_image = a*11B*tile_brightening_step
tile11_load_info = widget_label(tile11_load_field,value="Choose image for tile 2048", font="Helvetica", /align_center)
tile11_choose_file = widget_button(tile11_load_field,value="Choose file",uvalue="t11f", xsize = 100)
tile11_preview_area = widget_base(tiles_preview2,/row,/base_align_center)
tile11_desc = widget_label(tile11_preview_area, value="Tile 2048",font="Helvetica",xsize=100,/align_center)
tile11_preview = widget_draw(tile11_preview_area,xsize=2^p+2*marg_tile_prev,ysize=2^p+2*marg_tile_prev,uvalue="t11p",/button_events)

  widget_control, base, /realize
  widget_control, play_area, get_value=win
  widget_control, image_preview, get_value=prev_win
 
  widget_control, tile0_preview, get_value=tile0_win
  tile0 = {tile, load_field: tile0_load_field, choose_file: tile0_choose_file, filename: tile0_filename, load_info: tile0_load_info, preview_image: tile0_preview_image, cropped_image: tile0_cropped_image, tile_image: tile0_tile_image, tile_preview_window: tile0_win, preview_area: tile0_preview_area, desc: tile0_desc, preview: tile0_preview, x_val: 0.5, y_val: 0.5, z_val: 1., b_val: 0., c_val: 0., is_loaded: 0B}
 
  widget_control, tile1_preview, get_value=tile1_win
  tile1 = {tile, load_field: tile1_load_field, choose_file: tile1_choose_file, filename: tile1_filename, load_info: tile1_load_info, preview_image: tile1_preview_image, cropped_image: tile1_cropped_image, tile_image: tile1_tile_image, tile_preview_window: tile1_win, preview_area: tile1_preview_area, desc: tile1_desc, preview: tile1_preview, x_val: 0.5, y_val: 0.5, z_val: 1., b_val: 0., c_val: 0., is_loaded: 0B}

  widget_control, tile2_preview, get_value=tile2_win
  tile2 = {tile, load_field: tile2_load_field, choose_file: tile2_choose_file, filename: tile2_filename, load_info: tile2_load_info, preview_image: tile2_preview_image, cropped_image: tile2_cropped_image, tile_image: tile2_tile_image, tile_preview_window: tile2_win, preview_area: tile2_preview_area, desc: tile2_desc, preview: tile2_preview, x_val: 0.5, y_val: 0.5, z_val: 1., b_val: 0., c_val: 0., is_loaded: 0B}

  widget_control, tile3_preview, get_value=tile3_win
  tile3 = {tile, load_field: tile3_load_field, choose_file: tile3_choose_file, filename: tile3_filename, load_info: tile3_load_info, preview_image: tile3_preview_image, cropped_image: tile3_cropped_image, tile_image: tile3_tile_image, tile_preview_window: tile3_win, preview_area: tile3_preview_area, desc: tile3_desc, preview: tile3_preview, x_val: 0.5, y_val: 0.5, z_val: 1., b_val: 0., c_val: 0., is_loaded: 0B}

  widget_control, tile4_preview, get_value=tile4_win
  tile4 = {tile, load_field: tile4_load_field, choose_file: tile4_choose_file, filename: tile4_filename, load_info: tile4_load_info, preview_image: tile4_preview_image, cropped_image: tile4_cropped_image, tile_image: tile4_tile_image, tile_preview_window: tile4_win, preview_area: tile4_preview_area, desc: tile4_desc, preview: tile4_preview, x_val: 0.5, y_val: 0.5, z_val: 1., b_val: 0., c_val: 0., is_loaded: 0B}

  widget_control, tile5_preview, get_value=tile5_win
  tile5 = {tile, load_field: tile5_load_field, choose_file: tile5_choose_file, filename: tile5_filename, load_info: tile5_load_info, preview_image: tile5_preview_image, cropped_image: tile5_cropped_image, tile_image: tile5_tile_image, tile_preview_window: tile5_win, preview_area: tile5_preview_area, desc: tile5_desc, preview: tile5_preview, x_val: 0.5, y_val: 0.5, z_val: 1., b_val: 0., c_val: 0., is_loaded: 0B}

  widget_control, tile6_preview, get_value=tile6_win
  tile6 = {tile, load_field: tile6_load_field, choose_file: tile6_choose_file, filename: tile6_filename, load_info: tile6_load_info, preview_image: tile6_preview_image, cropped_image: tile6_cropped_image, tile_image: tile6_tile_image, tile_preview_window: tile6_win, preview_area: tile6_preview_area, desc: tile6_desc, preview: tile6_preview, x_val: 0.5, y_val: 0.5, z_val: 1., b_val: 0., c_val: 0., is_loaded: 0B}

  widget_control, tile7_preview, get_value=tile7_win
  tile7 = {tile, load_field: tile7_load_field, choose_file: tile7_choose_file, filename: tile7_filename, load_info: tile7_load_info, preview_image: tile7_preview_image, cropped_image: tile7_cropped_image, tile_image: tile7_tile_image, tile_preview_window: tile7_win, preview_area: tile7_preview_area, desc: tile7_desc, preview: tile7_preview, x_val: 0.5, y_val: 0.5, z_val: 1., b_val: 0., c_val: 0., is_loaded: 0B}

  widget_control, tile8_preview, get_value=tile8_win
  tile8 = {tile, load_field: tile8_load_field, choose_file: tile8_choose_file, filename: tile8_filename, load_info: tile8_load_info, preview_image: tile8_preview_image, cropped_image: tile8_cropped_image, tile_image: tile8_tile_image, tile_preview_window: tile8_win, preview_area: tile8_preview_area, desc: tile8_desc, preview: tile8_preview, x_val: 0.5, y_val: 0.5, z_val: 1., b_val: 0., c_val: 0., is_loaded: 0B}

  widget_control, tile9_preview, get_value=tile9_win
  tile9 = {tile, load_field: tile9_load_field, choose_file: tile9_choose_file, filename: tile9_filename, load_info: tile9_load_info, preview_image: tile9_preview_image, cropped_image: tile9_cropped_image, tile_image: tile9_tile_image, tile_preview_window: tile9_win, preview_area: tile9_preview_area, desc: tile9_desc, preview: tile9_preview, x_val: 0.5, y_val: 0.5, z_val: 1., b_val: 0., c_val: 0., is_loaded: 0B}

  widget_control, tile10_preview, get_value=tile10_win
  tile10 = {tile, load_field: tile10_load_field, choose_file: tile10_choose_file, filename: tile10_filename, load_info: tile10_load_info, preview_image: tile10_preview_image, cropped_image: tile10_cropped_image, tile_image: tile10_tile_image, tile_preview_window: tile10_win, preview_area: tile10_preview_area, desc: tile10_desc, preview: tile10_preview, x_val: 0.5, y_val: 0.5, z_val: 1., b_val: 0., c_val: 0., is_loaded: 0B}

  widget_control, tile11_preview, get_value=tile11_win
  tile11 = {tile, load_field: tile11_load_field, choose_file: tile11_choose_file, filename: tile11_filename, load_info: tile11_load_info, preview_image: tile11_preview_image, cropped_image: tile11_cropped_image, tile_image: tile11_tile_image, tile_preview_window: tile11_win, preview_area: tile11_preview_area, desc: tile11_desc, preview: tile11_preview, x_val: 0.5, y_val: 0.5, z_val: 1., b_val: 0., c_val: 0., is_loaded: 0B}

  tiles = {tiles, tile0: tile0, tile1: tile1, tile2: tile2, tile3: tile3,tile4: tile4, tile5: tile5, tile6: tile6, tile7: tile7, tile8: tile8, tile9: tile9, tile10: tile10, tile11: tile11}


  activate_image, 0B, 0B	
  			
  xmanager, 'play2048', base, /no_block
END
