pico-8 cartridge // http://www.pico-8.com
version 33
__lua__
function _init()
	start_engine()
		
	cls()
	--projmat:mul3x1(vec3d:create(1,0,1))

	--cube = object3d:create(unit_cube, vec3d:create(0,0,0))
	--cube:set_pos(vec3d:create(0,0,0))
	--add(objects3d, cube)

	fox_v=read_vector_string(fox_v_string)
	--print(fox_v[1].x)
    fox_f=read_face_string(fox_f_string)
	fox = object3d:create({verts=fox_v, tris=fox_f}, vec3d:create(0,0,40))
	add(objects3d, fox)
	
	--fountain_v=read_vector_string(fountain_v_string)
	--print(fox_v[1].x)
    --fountain_f=read_face_string(fountain_f_string)
	--fountain = object3d:create({verts=fountain_v, tris=fountain_f}, vec3d:create(0,0,25))
	--add(objects3d, fountain)

	--plane = object3d:create(plane, vec3d:create(0,0,10))
	--add(objects3d,plane)
end

function _update()
	
	local vforward = look_dir:multiply_scalar(1)
	
	if (btn(2)) camera3d=camera3d:add(vforward)
	if (btn(3)) camera3d=camera3d:sub(vforward)

	if (btn(0)) yaw += 1 --* time()
	if (btn(1)) yaw -= 1 --* time()

	if (btn(4)) camera3d.y+=1
	if (btn(5)) camera3d.y-=1

end

function _draw()
	cls(15)
	for object3d in all(objects3d) do
		--print(object3d.world_matrix[1][1])
		for mesh in all(object3d.meshes)do
			--print(object3d.world_matrix[3][1])
			mesh:draw("shaded", object3d.to_world_matrices)
		end
	end
	print(stat(1))
	--wireframe 0.07
	--shaded linefill 0.083
end

-->8

-->8
-- 3d rendering engine

function start_engine()
	fox_v_string="fe7106e700c6fec605aeffebfe1404b600d8fe6605e001f9fe710734027dfe4404b701b1ff8b0a06ffabff0b099affedff870a590009ff1b0a7f0055ff3d03c6fd3dfedd0350fdbdff2004dbfe4403060356fda9027d0504fdf4029a04a7ffc5fd5d0041fdf7fd0100ccfefbfd4000e7fd49feb102d6fd21fe2101d9fe0afded028afe43fe9d03acffb9feda048effb8fdf408580070fe9b08b7ffeffefa073e0022fe6403060010fe98039a0038fed8028bff84fe5b022e008afe2c0344012bfd1b0162fe5ffd7509720219fdd50a210283fe5d0a870114fc8f081b016dfc3608fb01c4029f0154fd2702ba02cbfce9fdb00947010d004d0a860072013d09ac01b5015508a100af01e905af002cfdb90110fc4600430775ffbdff36086effccfdbb09ec00e5005e069cff2d012006acffd000fa05ecfdbefdbf005ffc21fe9300d0fb63ff05033ffcd5034d0271ff3203360143fd73ff600bb0ffd6ff550bd70064ff6c0aef012400560a970157000609a3ffe3015f0484fc970007050bfd1dffc70acbffd1ffce06c0ff77fdc7008dfc96feaa03daff3cfda401c0fff2fd96011f000eff440272fc65ff090585ff69ffac061eff0e01d0061a00da017e06ce00affe0b07b80232fdbc07f20102021f007afc8e01050051fb7f01cd0278fbeefe2701d4ff6afd84019efeaafea201bdfd13fd360054ff40fec5032efe91033a0116ff22ff7903a5fcfdfe6801acfc32fd9c016dfd6301a8072f015afc9908430266ff870168fc77fef90aef00df012103dffc3afea0053c027e005502ecfbf9ffff00e9fb2401a300e000ad020f0053012a024d005f0018ffa6009d02ccffaf00b8017cfee100ce0195fe9400e3007dfe8d00ee0124fe8901fd012eff7c0b7c03dbffe20a5203d600460ad1033200dc0816030bffaf09ca03d0ff3f09d70333fef608c203c4ff71093c03acfe3c08570313fee5fffd027afe4d00920213009d014afbae011d08e3029600dc076402cdfeae026f019fff8f01b50166ffd7031b0199002103c80234ffdb00f00219006e00d80249ff3f0aa9034bffa40a4e036f029603370018015c025100e8026f021100f8fdc80093fff7012b01c301d900e903a6026f0116005b0151032f02210007007206a402e0ffb707e70375ff62050702f6fde0092d02a801f7051800f900df04f902e6017a044801fffdb7002701beff5b0ab40236ff690bb70361ffd10052fb3aff6104160230ff1e0a8a028bfd75fffcff59fd48fffcff2afd59fffcfe94fdd0fffc0193fe03fffc015dfdddfffc01f2fee1fffc0276feb6fffc008ffec4fffcfbe4fe1cfffc022bfee2fffc02790257fffcfd84021afffc00c1023ffffcfd44026cfffcff880024fffc02efff72fffc0311ffdffffcfb6c0272fffcfdd2026dfffcfd8e02a1fffcff2bfdadfffcfceefd82fffcfda9fe3dfffcfc20fe6afffcfc08fe7dfffc006afe05fffc01410093fffc016f004bfffc02dc0060fffcfb88fe93fffcfc030179fffcfc2000cffffc013c011dfffcfbc101a1fffc0138fee4fffc0274fee8fffc0266fee9fffc026afeeafffc026ffef1fffc02870203fffc0129"
	fox_f_string="01020301040504010306040307080909080a0b0c0d0e0f10111213141516170218191a1b1c1d1e1f201c21131222232425261927280e291a192a2b2c2d100f2e13212f1b30222431323334342d0f352e360c0b370e3839280f0e332d342a3a3b2a3c3d072a3e3e08073f40342a413a1926291b2f42400b0d35432e4417180a4109412a07451246470b40481b49021b484a2d4b014c4d3e30084e4f50270e39511c1e5115521e16511553525412111e4455284e5038563947570b160c144551521c2003200603322f333a410a582e59525859312408262229223129493234400d4902011b48490d2c2f3e2c5a4b4b332c1848441a301b01191b5b262532422f254d4c3d2b2a503f280c16550d44483e2f30440c55475c531453152152595116154b2d3303021717441e171e1d1c031d31081a301a081b4249014d1955161e180248332f2c4934405d3a0a3249424d25194c01053f340f3b3c2a2a2c3e29311a505e3f375747431113351143240a082112455f040628274e604740440d0c57370b475314471437525358461c45592e21140c3743132e283f0f4552215d3b3a171d03245d0a451c5161585361535c626364656667681f46696a686b6c6d6e6c6f707172057173746775766050776e78797a7b7c7d7e7f7080686a1f697567362e58380e10677a7969676a1f1c468182838468467e8586667a675b254c8782858883648446126774657d657e657d66898a8b058a715b738c8d5a8e775a2b108d818c232286858f857e87586136868f8e6287638287625688647569905e6040045f8b91926d7170238d828180706f614f9389788a8a058b8183886c6b802b6d6c6e728a828d8f56388894067b79206a7d7a66504f760941072b5a2c2391243d916d9123953d6d2b3d3c9171238c8c225b3881886b6d92957f6b956b9270726f6c806f6c6e776f726e7f9570952370718c738a786e8a72718b5f94867c7e764f61772b6c24915d5b4c735b2226785a778e5a894b5a4a5a8d4a8d2d4a8b05041254848f858247605c7b7d7c5d3c3b8382626a201f7b06790620796b7f80913c5d78895a06945f8e898b7c8e8b10813864836260765c8d8e8f947b7c8e7c86947c8b7b7a7d67796a5c76614c0573403f5e5e50602d8d10959291a09f9cb5b74fb54f4ea1a34ea14e27a9a127a92739989754985411aead35ae3536a79e93abac11ab1135a8a939a83956aaa856a3b54eac9811af9d68af6884bda665bd65749b9f759b7590a6a565b6b8879796849784549db068a2a464b09a69b06968b2b17e9a99909a90699fa0749f7475b3a793b3934f9eb4619e6193bea264be6463b4ae36b43661a4aa56a45664b1b687b1877e96af84999b90a5b27ea57e65b8be63b86387a09c74babb749cb974b9ba74adab35b7b34fbcbd74bbbc74"
	
	fountain_v_string="000000d1fe80000002a7fe80016d00d1ff89016d02a7ff8900e100d1013600e102a70136ff1e00d10136ff1e02a70136fe9200d1ff89fe9202a7ff890000045afc0b03c3045afec60253045a0333fdac045a0333fc3c045afec60000055ffc0b03c3055ffec60253055f0333fdac055f0333fc3c055ffec60000000efd74026b000eff36017e000e020ffe81000e020ffd94000eff36"
	fountain_f_string="0204030406050608070a0f0b0a0201080a090115190f1410080e0f060d0e040c0d020b0c1110140e13140d12130c11120b1011091918050718030517031615010203030405050607020a0b090a010708090901190b0f100a080f08060e06040d04020c1312111311140e0d130f0e140d0c120c0b11070918170518160317010315"

	objects3d = {}

	camera3d = vec3d:create(0,0,0)
	look_dir = vec3d:create(1,0,1)
	yaw = 0
	
	fnear = 0.1
	ffar = 1000
	ffov = 50.0
	faspect_ratio = 1
	ffovrad = 1.0/tan(ffov * 0.5 / 180.0 * 3.14)
	
	projmat = mat4x4:create({
		{faspect_ratio * ffovrad,0,0,0},
		{0,ffovrad,0,0},
		{0,0,ffar/(ffar-fnear),1},
		{0,0,(-ffar*fnear)/(ffar-fnear),0}
	})
end


engine2d = {}

function engine2d:outline_tri(tri, color)
	line(tri[1].x, tri[1].y, tri[2].x, tri[2].y, color)
	line(tri[3].x, tri[3].y, color)
	line(tri[1].x, tri[1].y, color)
end


function engine2d:fill_tri(tri, fill_pattern, color)
	local y_sorted_indices = ins_sort_3({	tri[1].y,
											tri[2].y,		
											tri[3].y
										})

	v1 = tri[y_sorted_indices[1]]
	v2 = tri[y_sorted_indices[2]]
	v3 = tri[y_sorted_indices[3]]			
	v4 = {x=(v1.x + ((v2.y - v1.y) / (v3.y - v1.y)) * (v3.x - v1.x)),
						y=v2.y }
	
	local invslope1=(v2.x-v1.x)/(v2.y-v1.y)
	local invslope2=(v4.x-v1.x)/(v4.y-v1.y)
	
	local curx1=v1.x
	local curx2=v1.x

	local starter=1-v1.y%1
	curx1 += starter*invslope1
	curx2 += starter*invslope2

	for y=flr(v1.y), flr(v2.y)-1 do
		rectfill(curx1, y, curx2, y, color)
		curx1 += invslope1
		curx2 += invslope2
	end    
	
	-- bottom tri
	invslope1 = (v2.x - v3.x) / (v2.y - v3.y)
	invslope2 = (v4.x - v3.x) / (v4.y - v3.y)
	
	curx1=v2.x
	curx2=v4.x

	starter=1-v4.y%1
	curx1 += starter*invslope1
	curx2 += starter*invslope2
		
	for y=flr(v4.y), flr(v3.y)-1 do
		rectfill(curx1, y, curx2, y, color)
		curx1 += invslope1
		curx2 += invslope2
	end 
end


function engine2d:fill_tri2(tri, fill_pattern, color)
	x1,y1,x2,y2,x3,y3 = tri[1].x, tri[1].y,tri[2].x,tri[2].y, tri[3].x,tri[3].y
	color1 = 3

	local min_x=min(x1,min(x2,x3))
	
	if(min_x>127)return
		local max_x=max(x1,max(x2,x3))
	if(max_x<0)return
		local min_y=min(y1,min(y2,y3))
	if(min_y>127)return
		local max_y=max(y1,max(y2,y3))
	if(max_y<0)return

	local x1=band(x1,0xffff)
	local x2=band(x2,0xffff)
	local y1=band(y1,0xffff)
	local y2=band(y2,0xffff)
	local x3=band(x3,0xffff)
	local y3=band(y3,0xffff)

	local width=min(127,max_x)-max(0,min_x)
	local height=min(127,max_y)-max(0,min_y)

	if(width>height)then --wide triangle 
		local nsx,nex
		--sort y1,y2,y3
		if(y1>y2)then
			y1,y2=y2,y1
			x1,x2=x2,x1
		end

		if(y1>y3)then
			y1,y3=y3,y1
			x1,x3=x3,x1
		end

		if(y2>y3)then
			y2,y3=y3,y2
			x2,x3=x3,x2 
		end

		if(y1!=y2)then 
			local delta_sx=(x3-x1)/(y3-y1)
			local delta_ex=(x2-x1)/(y2-y1)

			if(y1>0)then
				nsx=x1
				nex=x1
				min_y=y1
			else --top edge clip
				nsx=x1-delta_sx*y1
				nex=x1-delta_ex*y1
				min_y=0
			end

			max_y=min(y2,128)

			for y=min_y,max_y-1 do

				rectfill(nsx,y,nex,y,color1)
				nsx+=delta_sx
				nex+=delta_ex
			end

		else --where top edge is horizontal
			nsx=x1
			nex=x2
		end

		if(y3!=y2)then
			local delta_sx=(x3-x1)/(y3-y1)
			local delta_ex=(x3-x2)/(y3-y2)

			min_y=y2
			max_y=min(y3,128)
			if(y2<0)then
				nex=x2-delta_ex*y2
				nsx=x1-delta_sx*y1
				min_y=0
			end

			for y=min_y,max_y do
				rectfill(nsx,y,nex,y,color1)
				nex+=delta_ex
				nsx+=delta_sx
			end

		else --where bottom edge is horizontal
			rectfill(nsx,y3,nex,y3,color1)
		end
	else --tall triangle -----------------------------------<><>----------------
		local nsy,ney

		--sort x1,x2,x3
		if(x1>x2)then
		x1,x2=x2,x1
		y1,y2=y2,y1
		end

		if(x1>x3)then
		x1,x3=x3,x1
		y1,y3=y3,y1
		end

		if(x2>x3)then
		x2,x3=x3,x2
		y2,y3=y3,y2 
		end

		if(x1!=x2)then 
		local delta_sy=(y3-y1)/(x3-x1)
		local delta_ey=(y2-y1)/(x2-x1)

		if(x1>0)then
		nsy=y1
		ney=y1
		min_x=x1
		else --top edge clip
		nsy=y1-delta_sy*x1
		ney=y1-delta_ey*x1
		min_x=0
		end

		max_x=min(x2,128)

		for x=min_x,max_x-1 do

		rectfill(x,nsy,x,ney,color1)
		nsy+=delta_sy
		ney+=delta_ey
		end

		else --where top edge is horizontal
		nsy=y1
		ney=y2
		end

		if(x3!=x2)then
		local delta_sy=(y3-y1)/(x3-x1)
		local delta_ey=(y3-y2)/(x3-x2)

		min_x=x2
		max_x=min(x3,128)
		if(x2<0)then
		ney=y2-delta_ey*x2
		nsy=y1-delta_sy*x1
		min_x=0
		end

		for x=min_x,max_x do

		rectfill(x,nsy,x,ney,color1)
		ney+=delta_ey
		nsy+=delta_sy
		end

		else --where bottom edge is horizontal

		rectfill(x3,nsy,x3,ney,color1)

		end

		end
		end

		-- by @scgrn   
		-- draws a filled convex polygon
		-- v is an array of vertices
		-- {x1, y1, x2, y2} etc
		function render_poly(v,col)
		col=col or 5

		-- initialize scan extents
		-- with ludicrous values
		local x1,x2={},{}
		for y=0,127 do
		x1[y],x2[y]=128,-1
		end
		local y1,y2=128,-1

		-- scan convert each pair
		-- of vertices
		for i=1, #v/2 do
		local next=i+1
		if (next>#v/2) next=1

		-- alias verts from array
		local vx1=flr(v[i*2-1])
		local vy1=flr(v[i*2])
		local vx2=flr(v[next*2-1])
		local vy2=flr(v[next*2])

		if vy1>vy2 then
		-- swap verts
		local tempx,tempy=vx1,vy1
		vx1,vy1=vx2,vy2
		vx2,vy2=tempx,tempy
		end 

		-- skip horizontal edges and
		-- offscreen polys
		if vy1~=vy2 and vy1<128 and
		vy2>=0 then

		-- clip edge to screen bounds
		if vy1<0 then
			vx1=(0-vy1)*(vx2-vx1)/(vy2-vy1)+vx1
			vy1=0
		end
		if vy2>127 then
			vx2=(127-vy1)*(vx2-vx1)/(vy2-vy1)+vx1
			vy2=127
		end
			
		-- iterate horizontal scans
		for y=vy1,vy2 do
			if (y<y1) y1=y
			if (y>y2) y2=y

			-- calculate the x coord for
			-- this y coord using math!
			x=(y-vy1)*(vx2-vx1)/(vy2-vy1)+vx1

			if (x<x1[y]) x1[y]=x
			if (x>x2[y]) x2[y]=x
		end 
		end
		end

		-- render scans
		for y=y1,y2 do
		local sx1=flr(max(0,x1[y]))
		local sx2=flr(min(127,x2[y]))

		local c=col*16+col
		local ofs1=flr((sx1+1)/2)
		local ofs2=flr((sx2+1)/2)
		memset(0x6000+(y*64)+ofs1,c,ofs2-ofs1)
		pset(sx1,y,c)
		pset(sx2,y,c)
		end 
end


vec3d = {x=0,y=0,z=0,w=1}

function vec3d:create(x,y,z,w)
	self.__index = self
	return setmetatable({
		x = x,
		y = y,
		z = z,
		w = 1
	} or {}, self)
end 

function vec3d:divide_scalar(scalar)
	v = vec3d:create()
	v.x = self.x / scalar
	v.y = self.y / scalar
	v.z = self.z / scalar
	v.w = self.w 

	return v
end 

function vec3d:multiply_scalar(scalar)
	v = vec3d:create()
	v.x = self.x * scalar
	v.y = self.y * scalar
	v.z = self.z * scalar
	v.w = self.w

	return v
end 

function vec3d:add(vec3d_2)
	v = vec3d:create()
	v.x = self.x + vec3d_2.x
	v.y = self.y + vec3d_2.y
	v.z = self.z + vec3d_2.z
	v.w = self.w
	return v
end

function vec3d:sub(vec3d_2)
	v = vec3d:create()
	v.x = self.x - vec3d_2.x
	v.y = self.y - vec3d_2.y
	v.z = self.z - vec3d_2.z
	v.w = self.w
	return v
end

function vec3d:length()
	return sqrt(self:dot_product(self));
end

function vec3d:normalise()
	v = vec3d:create()
	local l = self:length();
	v.x = self.x / l
	v.y = self.y / l
	v.z = self.z / l
	v.w = self.w
	return v
end

function vec3d:mul3x1(vec3d_2)
	v = vec3d:create()
	v.x = self.x * vec3d_2.x
	v.y = self.y * vec3d_2.y
	v.z = self.z * vec3d_2.z
	v.w = self.w
	return v
end

function vec3d:dot_product(vec3d_2)
	return self.x*vec3d_2.x + self.y*vec3d_2.y + self.z*vec3d_2.z 
end

function vec3d:cross_product(vec3d_2)
	v = vec3d:create()
	v.x = self.y * vec3d_2.z - self.z * vec3d_2.y
	v.y = self.z * vec3d_2.x - self.x * vec3d_2.z
	v.z = self.x * vec3d_2.y - self.y * vec3d_2.x
	v.w = self.w
	return v
end

mesh = {}

function mesh:create(verts, tris, child_matrices)
	self.__index = self

	return setmetatable({verts=verts, tris=tris, child_matrices=child_matrices} or {}, self)
end

function mesh:draw (mode, to_world_matrices)
	local transformed_vertices = {}
	local projected_vertices = {}
	local viewed_vertices = {}
	local raster_tris = {}
	-- to_world

	local v_up=vec3d:create(0,1,0)
	local target = vec3d:create(0,0,1)
	local mat_camera_rot =  mat4x4:create_rot_y(yaw)
	--local look_dir = camera3d:add(target)
	look_dir = mat_camera_rot:mul3x1(target)
	target = camera3d:add(look_dir)

	local mat_camera = mat4x4:create_point_at(camera3d, target, v_up)
	local mat_view = mat_camera:quick_inverse()

	for vertex in all(self.verts) do	
		trans_vec = to_world_matrices[1]:mul3x1(vertex)
		add(transformed_vertices, trans_vec)
	end

	for tri in all(self.tris) do	
		line1 = transformed_vertices[tri[2]]:sub(transformed_vertices[tri[1]])
		line2 = transformed_vertices[tri[3]]:sub(transformed_vertices[tri[1]])
	
		local normal = line1:cross_product(line2)
		normal = normal:normalise()

		local camera_ray = transformed_vertices[tri[1]]:sub(camera3d)

		if(normal:dot_product(camera_ray) < 0)then
			local triViewed = {}

			triViewed[1] = mat_view:mul3x1(transformed_vertices[tri[1]])
			triViewed[2] = mat_view:mul3x1(transformed_vertices[tri[2]])
			triViewed[3]  = mat_view:mul3x1(transformed_vertices[tri[3]])

			local clipped_triangles = triangle_clip_plane(vec3d:create( 0.0, 0.0, 1), vec3d:create(0.0, 0.0, 1.0), triViewed)

			for clipped_tri in all(clipped_triangles) do
				local triProjected = {}
				triProjected[1] = projmat:mul3x1(clipped_tri[1])
				triProjected[2] = projmat:mul3x1(clipped_tri[2])
				triProjected[3] = projmat:mul3x1(clipped_tri[3])

				
				triProjected[1] = triProjected[1]:divide_scalar(triProjected[1].w)
				triProjected[2] = triProjected[2]:divide_scalar(triProjected[2].w)
				triProjected[3] = triProjected[3]:divide_scalar(triProjected[3].w)

				triProjected[1].x *= -1.0;
				triProjected[2].x *= -1.0;
				triProjected[3].x *= -1.0;
				triProjected[1].y *= -1.0;
				triProjected[2].y *= -1.0;
				triProjected[3].y *= -1.0;

				vOffsetView = vec3d:create( 1,1,0 );
				
				triProjected[1] = triProjected[1]:add(vOffsetView)
				triProjected[2] = triProjected[2]:add(vOffsetView)
				triProjected[3] = triProjected[3]:add(vOffsetView)
				triProjected[1].x *= 0.5 * 128;
				triProjected[1].y *= 0.5 * 128;
				triProjected[2].x *= 0.5 * 128;
				triProjected[2].y *= 0.5 * 128;
				triProjected[3].x *= 0.5* 128;
				triProjected[3].y *= 0.5 * 128;

				triProjected.z_mid = (transformed_vertices[tri[1]].z + transformed_vertices[tri[2]].z + transformed_vertices[tri[3]].z)/3.0
				
				if(mode == "wireframe")then
					engine2d:outline_tri(triProjected, 3)
				end
				
				if(mode == "shaded" or mode == "shaded_wireframe")then
					light_dir = vec3d:create(0,0,-1)
				
					l = sqrt(light_dir.x*light_dir.x + 
														light_dir.y*light_dir.y +
														light_dir.z*light_dir.z)
					
					light_dir.x /= l
					light_dir.y /= l
					light_dir.z /= l
					
					dp = normal.x * light_dir.x +
										normal.y * light_dir.y +
										normal.z * light_dir.z

					local fill_pattern = 0b00000000000000000	
					local tri_color = 14								
					if(dp<0.8)then
						tri_color = 8	
					end
					if(dp<0.6)then
						tri_color =2
					end
					if(dp<0.1)then

					end
					
					triProjected.color = tri_color

					add(raster_tris, triProjected)
				end
			end
		end
	end 

	-- painters algo
	--sorted_tris = raster_tris
	
	quicksort(raster_tris)
	for sorted_tri_i=#raster_tris,1,-1 do
		engine2d:fill_tri(raster_tris[sorted_tri_i], fill_pattern, raster_tris[sorted_tri_i].color) 
	end

	--x[[ TOP-BOTTOM CLIPPING
	for sorted_tri_i=#raster_tris,1,-1 do
		local listTriangles = {};

		add(listTriangles, raster_tris[sorted_tri_i]);
		local nNewTriangles = 1;

		for p = 1,2 do
			local trisToAdd = {}
			while nNewTriangles > 0 do
				local test = deli(listTriangles, 1);
				nNewTriangles -= 1

				if p == 1 then
					trisToAdd = triangle_clip_plane(vec3d:create( 0.0, 0.0, 0.0 ), vec3d:create( 0.0, 1.0, 0.0 ), test)
				end

				if p == 2 then
					trisToAdd = triangle_clip_plane(vec3d:create( 0.0, 128 - 1, 0.0 ), vec3d:create( 0.0, -1.0, 0.0 ), test)
				end

				for tri_to_add in all(trisToAdd) do
					add(listTriangles, tri_to_add);
				end
			end
			nNewTriangles = #listTriangles
		end

		for triangle_to_draw in all (listTriangles) do
			--print(triangle_to_draw.color)
			engine2d:fill_tri(triangle_to_draw, fill_pattern, triangle_to_draw.color) 

			if(mode == "shaded_wireframe")then
				fillp(0b00000000000000000)
				engine2d:outline_tri(triangle_to_draw[sorted_tri_i], 1)
			end
		end
	end
	--]]
end

function quicksort(t, start, endi)
   start, endi = start or 1, endi or #t
  --partition w.r.t. first element
  if(endi - start < 1) then return t end
  local pivot = start
  for i = start + 1, endi do
    if t[i].z_mid <= t[pivot].z_mid then
      if i == pivot + 1 then
        t[pivot],t[pivot+1] = t[pivot+1],t[pivot]
      else
        t[pivot],t[pivot+1],t[i] = t[i],t[pivot],t[pivot+1]
      end
      pivot = pivot + 1
    end
  end
   t = quicksort(t, start, pivot - 1)
  return quicksort(t, pivot + 1, endi)
end

object3d ={
	--mesh=mesh:create(unit_cube),
	--origin=vec3d:create(0,0,0)
}

function object3d:create(template_mesh, origin)
	self.__index = self
	--print(unpack(template_mesh))
	local translate_matrix = mat4x4:create({{1,0,0,0},
								{0,1,0,0},
								{0,0,1,0},
								{origin.x,origin.y,origin.z,1}})

	return setmetatable({
		current_pos=origin,
		current_rot=vec3d:create(),
		meshes={mesh:create(template_mesh.verts, template_mesh.tris, {nil,nil,nil})},
		to_world_matrices={translate_matrix,nil,nil}
	} or {}, self)
end

function object3d:set_pos(mat3x1)
	current_pos = mat3x1
	transform_matrix = mat4x4:create()
	transform_matrix[1][4] = mat3x1.x
	transform_matrix[2][4] = mat3x1.y
	transform_matrix[3][4] = mat3x1.z
	self.mesh:transform(transform_matrix) 
end

function object3d:rotate(factor)
	--local rot_mat = mat4x4:create()
	self.current_rot.y += 5
	local rot_mat = mat4x4:create()
	
	rot_mat[1][1] = cos((self.current_rot.y%360)/360)
	rot_mat[3][1] = -sin((self.current_rot.y%360)/360)
	rot_mat[1][3] = sin((self.current_rot.y%360)/360)
	rot_mat[3][3] = cos((self.current_rot.y%360)/360)
	--print(self.world_matrix[4][3])
	self.to_world_matrices[2] = rot_mat
	--print(self.world_matrix[4][3])
end

-- xx debug 40 tokens xx
function mesh:print ()
	for i_tri=1, #self do
		print("--\n")
		for vec in all (self[i_tri]) do
			print("x"..vec.x..
								" y"..vec.y..
								" z"..vec.z,
								i_tri)
		end
	end
end
-- xxxxxxxxxxxxxxxxxxxx

-->8
-- primitive shapes

unit_cube = {
		verts = {
					vec3d:create(0,0,0), 
					vec3d:create(0,0,1),
					vec3d:create(0,1,0),
					vec3d:create(0,1,1),
					vec3d:create(1,0,0),
					vec3d:create(1,0,1),
					vec3d:create(1,1,0),
					vec3d:create(1,1,1),
				},
		tris = {
					{1,7,5},
					{1,3,7},
					{1,4,3},
					{1,2,4},
					{3,8,7},
					{3,4,8},
					{5,7,8},
					{5,8,6},
					{1,5,6},
					{1,6,2},
					{2,6,8},
					{2,8,4},
		}
}

plane = {
		verts = {
					vec3d:create(0,0,0), 
					vec3d:create(0,0,100),
					vec3d:create(100,0,100),
					vec3d:create(100,0,0),
				},
		tris = {
					{1,2,3},
					{3,4,1},
		}
}

-->8
-- maths

mat4x4 = {
	{1,0,0,0},
	{0,1,0,0},
	{0,0,1,0},
	{0,0,0,1},
}

function mat4x4:create(mat)
	self.__index = self
	return setmetatable(mat or {}, self)
end

function mat4x4:create_point_at(pos, target, up)
	self.__index = self

	local new_forward = target:sub(pos)
	new_forward = new_forward:normalise()
	
	local a = new_forward:multiply_scalar(up:dot_product(new_forward))
	local new_up = up:sub(a)
	new_up = new_up:normalise()

	local new_right = new_up:cross_product(new_forward)

	local matrix = mat4x4:create()
	matrix[1][1] = new_right.x	
	matrix[1][2] = new_right.y	
	matrix[1][3] = new_right.z
	matrix[1][4] = 0.0
	matrix[2][1] = new_up.x		
	matrix[2][2] = new_up.y		
	matrix[2][3] = new_up.z		
	matrix[2][4] = 0.0
	matrix[3][1] = new_forward.x	
	matrix[3][2] = new_forward.y	
	matrix[3][3] = new_forward.z	
	matrix[3][4] = 0.0
	matrix[4][1] = pos.x			
	matrix[4][2] = pos.y		
	matrix[4][3] = pos.z	
	matrix[4][4] = 1.0

	return setmetatable(matrix or {}, self)
end

function mat4x4:create_rot_y(angle)
	self.__index = self

	local rot_mat = mat4x4:create({{1,0,0,0},
									{0,1,0,0},
									{0,0,1,0},
									{0,0,0,1}})

	--x[[
	rot_mat[1][1] = cos((angle%360)/360);
	rot_mat[1][3] = sin((angle%360)/360);
	rot_mat[3][1] = -sin((angle%360)/360);
	rot_mat[2][2] = 1.0;
	rot_mat[3][3] = cos((angle%360)/360);
	rot_mat[4][4] = 1.0;
	--]]
	return setmetatable(rot_mat or {}, self)
end

function mat4x4:mul3x1(mat3x1)
	local o = vec3d:create()

	o.x = mat3x1.x * self[1][1] + 
			mat3x1.y * self[2][1] + 
			mat3x1.z * self[3][1] + 
			mat3x1.w * self[4][1] 
							
	o.y = mat3x1.x * self[1][2] + 
							mat3x1.y * self[2][2] + 
							mat3x1.z * self[3][2] + 
							mat3x1.w * self[4][2] 
	
	o.z = mat3x1.x * self[1][3] + 
							mat3x1.y * self[2][3] + 
							mat3x1.z * self[3][3] + 
							mat3x1.w * self[4][3] 

				
	o.w = mat3x1.x * self[1][4] + 
					mat3x1.y * self[2][4] + 
					mat3x1.z * self[3][4] + 
					mat3x1.w * self[4][4] 

	--o.x /= o.w	
	--o.y /= o.w	
	--o.z /= o.w				
	return o
end

function mat4x4:mul4x4(mat4x4)
	local matrix = mat4x4:create({
			{1,0,0,0},
			{0,1,0,0},
			{0,0,1,0},
			{0,0,0,1},
		});

	for c = 1, 5 do
		for r = 1, 5 do
			matrix[r][c] = self[r][1] * mat4x4[1][c] + self[r][2] * mat4x4[2][c] + self[r][3] * mat4x4[3][c] + self[r][4] * mat4x4[4][c];
		end
	end
	return matrix;
end

function mat4x4:quick_inverse()
	local matrix = mat4x4:create({
			{1,0,0,0},
			{0,1,0,0},
			{0,0,1,0},
			{0,0,0,1},
		});
	matrix[1][1] = self[1][1] 
	matrix[1][2] = self[2][1] 
	matrix[1][3] = self[3][1] 
	matrix[1][4] = 0.0
	matrix[2][1] = self[1][2] 
	matrix[2][2] = self[2][2] 
	matrix[2][3] = self[3][2] 
	matrix[2][4] = 0.0
	matrix[3][1] = self[1][3] 
	matrix[3][2] = self[2][3]
	matrix[3][3] = self[3][3]
	matrix[3][4] = 0.0
	matrix[4][1] = -(self[4][1] * matrix[1][1] + self[4][2] * matrix[2][1] + self[4][3] * matrix[3][1])
	matrix[4][2] = -(self[4][1] * matrix[1][2] + self[4][2] * matrix[2][2] + self[4][3] * matrix[3][2])
	matrix[4][3] = -(self[4][1] * matrix[1][3] + self[4][2] * matrix[2][3] + self[4][3] * matrix[3][3])
	matrix[4][4] = 1.0;
	return matrix
end

function ins_sort_3(a)
	local indices = {1,2,3}
	for i=0,#a do
		local value = a[i]
		local j = i-1
		while j >= 0 and a[j] != nil and a[j] > value do
			a[j+1] = a[j]
			
			local temp_index = indices[j+1]
			indices[j+1] = indices[j]
			indices[j] = temp_index
			
			j = j-1
		end
		
		a[j+1] = value
	end
	
	return indices
end

function tan(a) return sin(a)/cos(a) end

function intersect_plane(plane_p, plane_n, line_start, line_end)
	plane_n = plane_n:normalise()
	local plane_d = -plane_n:dot_product(plane_p)
	local ad = line_start:dot_product(plane_n)
	local bd = line_end:dot_product(plane_n)
	local t = (-plane_d - ad) / (bd - ad);
	local line_start_to_end = line_end:sub(line_start)
	local line_to_intersect = line_start_to_end:multiply_scalar(t)
	return line_start:add(line_to_intersect)
end

function dist (p, plane_n, plane_p)
	local n = p:normalise()
	return (plane_n.x * p.x + plane_n.y * p.y + plane_n.z * p.z - plane_n:dot_product(plane_p));
end

function triangle_clip_plane(plane_p, plane_n, in_tri)
	local out_tri1 = vec3d:create()
	local out_tri2 = vec3d:create()
	clipped_triangles = {}
	plane_n = plane_n:normalise()

	inside_points_indices={}  
	local nInsidePointCurrentIndex = 1
	outside_points_indices={} 
	local nOutsidePointCurrentIndex = 1

	local d0 = dist(in_tri[1], plane_n, plane_p)
	local d1 = dist(in_tri[2], plane_n, plane_p)
	local d2 = dist(in_tri[3], plane_n, plane_p)

	if (d0 >= 0) then
	 	inside_points_indices[nInsidePointCurrentIndex] = 1
		nInsidePointCurrentIndex+=1
	else 
		outside_points_indices[nOutsidePointCurrentIndex] = 1
		nOutsidePointCurrentIndex+=1
	end

	if (d1 >= 0) then
	 	inside_points_indices[nInsidePointCurrentIndex] = 2
		nInsidePointCurrentIndex+=1
	else 
		outside_points_indices[nOutsidePointCurrentIndex] = 2
		nOutsidePointCurrentIndex+=1
	end

	if (d2 >= 0) then
	 	inside_points_indices[nInsidePointCurrentIndex] = 3
		nInsidePointCurrentIndex+=1
	else 
		outside_points_indices[nOutsidePointCurrentIndex] = 3
		nOutsidePointCurrentIndex+=1
	end

	if (nInsidePointCurrentIndex == 1)then
		return {}
	end

	if (nInsidePointCurrentIndex == 4)then
		out_tri1=in_tri
		return {out_tri1}
	end

	if (nInsidePointCurrentIndex == 2 and nOutsidePointCurrentIndex == 3) then
		out_tri1.color =  in_tri.color

		out_tri1[1] =  in_tri[inside_points_indices[1]];

		out_tri1[2] = intersect_plane(plane_p, plane_n, in_tri[inside_points_indices[1]], in_tri[outside_points_indices[1]]);
		out_tri1[3] = intersect_plane(plane_p, plane_n, in_tri[inside_points_indices[1]], in_tri[outside_points_indices[2]]);

		return {out_tri1}
	end

	if (nInsidePointCurrentIndex == 3 and nOutsidePointCurrentIndex == 2)then
		out_tri1.color =  in_tri.color;

		out_tri2.color =  in_tri.color;

		out_tri1[1] = in_tri[inside_points_indices[1]]
		out_tri1[2] = in_tri[inside_points_indices[2]]
		out_tri1[3] = intersect_plane(plane_p, plane_n, in_tri[inside_points_indices[1]], in_tri[outside_points_indices[1]]);

		out_tri2[1] = in_tri[inside_points_indices[2]]
		out_tri2[2] = out_tri1[3];
		out_tri2[3] = intersect_plane(plane_p, plane_n, in_tri[inside_points_indices[2]], in_tri[outside_points_indices[1]]);

		return {out_tri1, out_tri2}
	end
end
-->8

--Electric Gryphon's hex string data handling--

hex_string_data = "0123456789abcdef"
char_to_hex = {}
for i=1,#hex_string_data do
    char_to_hex[sub(hex_string_data,i,i)]=i-1
end

function read_byte(string)
    return char_to_hex[sub(string,1,1)]*16+char_to_hex[sub(string,2,2)]
end

function read_2byte_fixed(string)
    local a=read_byte(sub(string,1,2))
    local b=read_byte(sub(string,3,4))
    local val =a*256+b
    return val/256
end

cur_string=""
cur_string_index=1
function load_string(string)
    cur_string=string
    cur_string_index=1
end

function read_vector()
    v={}
    for i=1,3 do
        text=sub(cur_string,cur_string_index,cur_string_index+4)
        value=read_2byte_fixed(text)
        v[i]=value
        cur_string_index+=4
    end
    return v
end

function read_face()
    f={}
    for i=1,3 do
        text=sub(cur_string,cur_string_index,cur_string_index+2)
        value=read_byte(text)
        f[i]=value
        cur_string_index+=2
    end
    return f
end       

function read_vector_string(string)
    vector_list={}
    load_string(string)
    while(cur_string_index<#string)do
        vector=read_vector()
        add(vector_list,vec3d:create(vector[1],vector[2],vector[3]))
    end
    return vector_list
end

function read_face_string(string)
    face_list={}
    load_string(string)
    while(cur_string_index<#string)do
        face=read_face()
        add(face_list,face)
    end
        return face_list
end
__gfx__
00000000070707070000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000707070700000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00700700070707070000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00077000707070700000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00077000070707070000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00700700707070700000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000070707070000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000707070700000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
__map__
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000001010101010101000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000101010101010001010100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000001010101010101010100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000010000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
