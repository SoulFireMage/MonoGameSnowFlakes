open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Input
open System
 
module Text =
   open System.IO
   open System.Drawing
   open System.Windows.Forms
   //Build a new bitmap with it's own F#
 
   let bitmapSave (bitmap :Bitmap) =
      let stream = new System.IO.MemoryStream()
      bitmap.Save(stream, Imaging.ImageFormat.Png)
      bitmap.Dispose()
      stream.Position <- 0L
      stream
   
   let getGraphics (h:int)(w:int) =
      let bitmap = new Bitmap(w,h)
      let g = Graphics.FromImage(bitmap)
      g.SmoothingMode <- Drawing2D.SmoothingMode.AntiAlias
      g.TextRenderingHint <- Text.TextRenderingHint.ClearTypeGridFit
      g.Clear(Color.Transparent)
      (g ,bitmap)

   let toBitmapStream text   =
      let maxFont = 15.0f
      use font = new Font(FontFamily.GenericSansSerif, maxFont)
      let size =  TextRenderer.MeasureText(text, font)
      let g, bitmap = getGraphics size.Width size.Height
      //Each new Bitmap has it's own (x,y) so I use this fact to simply add more F#'s to get a quasi 3D effect.
      let cols =  [0 .. 5] |> List.zip [0 .. 5] |> List.iter(fun i -> let  x,y =  i
                                                                      TextRenderer.DrawText(g, text,font, new Point(x,y), Color.White))
      bitmapSave bitmap

   //There's a good reason I called it a pflakeBuilder - Thanks P Trelford :)
   let pflakeBuilder seed =
      let height,width=50,50
      let g, bitmap = getGraphics height width
      use brush = new SolidBrush(Color.Transparent)
      g.FillRectangle(brush, 0, 0, width, height)
      g.TranslateTransform((float32 width/2.0f), (float32 height/2.0f))
      let color = Color.FromArgb(128,255,255,255)
      use brush = new SolidBrush(color)
      let rand = Random()
      let polys =
            [for i in 1..12 ->
                let w = rand.Next(seed)+1 // width
                let h = rand.Next(seed)+1 // height
                let m = rand.Next(h)    // midpoint
                let s = rand.Next(seed + 10)   // start
                [|0,s; -w,s+m; 0,s+h; w,s+m|]
            ]
      for i in 0.0..60.0..300.0 do
           g.RotateTransform(float32 60.0)
           let poly points =
               let points = [|for (x,y) in points -> Point(x*5,y*5)|]
               g.FillPolygon(brush,points)
           polys |> List.iter poly 
      bitmapSave bitmap
   
let w, h = 1600.0, 1200.0
let rnd = System.Random()
let mutable speed = 0.5 
let mutable wind = 45.0
let mutable flakeScarcity = 3 
//I needed a double random min max function
let GetNextDoubleRnd min max = float32 (rnd.NextDouble() * (max - min) + min)

type innerFlake = {texture2d: Texture2D; 
                                mutable X: float; 
                                mutable Y: float; 
                                mutable V: float; //Velocity
                                mutable A: float;  //Angle or wind
                                scale : Nullable<Vector2>;
                                mutable spinfactor: float32 ; spinFun :(float32 -> float32 -> float32)  }

type flake = { mutable X:float; mutable Y:float; V:float; A:float; scale:Nullable<Vector2>;mutable spinfactor: float32 ; spinFun :(float32 -> float32 -> float32) ; innerflake: innerFlake}

type Snowflakes () as this =
   inherit Game ()
   let graphics = new GraphicsDeviceManager(this)
   do graphics.PreferredBackBufferWidth <- int w
   do graphics.PreferredBackBufferHeight <- int h
   let mutable spriteBatch : SpriteBatch = null
   let mutable textTexture : Texture2D = null
  
   let flakes = ResizeArray<_>()

   override this.LoadContent() =      
      spriteBatch <- new SpriteBatch(this.GraphicsDevice)
      use textStream = Text.toBitmapStream "F#"
      textTexture <- Texture2D.FromStream(this.GraphicsDevice, textStream)
 
   override this.Update(gameTime) =
     
      //use keys to see a blizzard :)   
      let HandleKeys K =
             match K with
             | Keys.Up -> if speed <= 15.0 then speed <- speed + 0.1
             | Keys.Down -> if speed >= 0.1 then speed <- speed - 0.1
             | Keys.Left -> if wind <= 35.0 then wind <- wind + 0.2
             | Keys.Right->  if wind >= -35.0 then wind <- wind - 0.2 
             | Keys.Space ->  if flakeScarcity >= 1 then flakeScarcity <- flakeScarcity  - 1
             | Keys.F -> if flakeScarcity <= 5 then flakeScarcity <- flakeScarcity  + 1
             | _ -> wind <- wind
 
      if Keyboard.GetState().GetPressedKeys().Length > 0 then HandleKeys (Keyboard.GetState().GetPressedKeys().[0]) |> ignore
      //Alter the scarcity to increase odds of getting more snowflakes
      
      if (rnd.Next(flakeScarcity)= 0) then 
         //Lets get both the F# flake AND unique Phil flakes in here. It's a cheap hack
         let flakePosition seed = 
               let x, y = rnd.NextDouble() * w, -16.0
               let a = wind + rnd.NextDouble() * 90.0 //(wind / 2.0)
               let v = (speed + rnd.NextDouble())*2.0
               let sfactor = (GetNextDoubleRnd 0.05 1.5)
               let spinfactor = GetNextDoubleRnd -0.01 0.01
               let spinFunction = if rnd.Next(2) % 2 = 0 then fun x y-> x + (y / 100.0f) else fun x y -> x - (y / 100.0f)
               let scale = System.Nullable(new Vector2( sfactor, sfactor))
               (x,y,v,a, scale, spinfactor, spinFunction)

         //Needed two sets of positions/scale and so on. And yes, the seed param is unused but it forces us to get a new random set of values.
         let x,y,v,a,scale,spinFactor, spinFun = flakePosition 4
        
         let x2, y2, v2, a2, scale2, spinFactor2, spinFun2 = flakePosition 26

         //The innerflake is merely ensuring we get a unique flake each time!
         let flake = { X=x; Y=y; V=v; A=a;scale=scale;spinfactor=spinFactor;spinFun= spinFun
                       innerflake={texture2d=Texture2D.FromStream(this.GraphicsDevice, Text.pflakeBuilder (rnd.Next(3)));
                       X=x2; Y=y2; V=v2; A=a2;scale=scale2;spinfactor=spinFactor2;spinFun=spinFun2}}
         flakes.Add(flake)         
      //Which flakes are on the screen? Which have gone off. As there is an innerflake, lets make sure they're both in shot
      let withInBounds x y = (x < w && x >= 0.) || (y < h && y >= 0.)
      let onScreen, offScreen = flakes |> Seq.toList |> List.partition (fun flake ->   withInBounds flake.X flake.Y && withInBounds flake.innerflake.X flake.innerflake.Y)  
      
      for flake in onScreen do
         let r = flake.A * System.Math.PI / 180.0
         let r2 = flake.innerflake.A * System.Math.PI / 180.0
         //Move flakes across the screen
         flake.X <- flake.X + ((cos r) * flake.V)
         flake.innerflake.X <- flake.innerflake.X  + ((cos r2) * flake.innerflake.V)
         //Move flakes down the screen
         flake.Y <- flake.Y + ((sin r) * flake.V)        
         flake.innerflake.Y  <- flake.innerflake.Y   + ((sin r2) * flake.innerflake.V)
      for flake in offScreen do
         flakes.Remove(flake) |> ignore

   override this.Draw(gameTime) =
      this.GraphicsDevice.Clear Color.Black
      
      spriteBatch.Begin(SpriteSortMode.Deferred, BlendState.AlphaBlend)
      let origin = Nullable(Vector2(float32 textTexture.Width/2.0f, float32 textTexture.Height/2.0f))
     
      //I wanted backwards spins as well as forwards, this is a slightly complex way todo this.
      for flake in flakes do
            let secondFlakeValues = flake.innerflake
            let secondFlake = flake.innerflake.texture2d
            //decompose the spin function and value to allow incrementing
            let  spinFun = flake.spinFun
            flake.spinfactor <- spinFun  flake.spinfactor (float32 flake.V)
           
            let spinFun2 = secondFlakeValues.spinFun
          
            secondFlakeValues.spinfactor <- spinFun2 secondFlakeValues.spinfactor (float32 secondFlakeValues.V)
            //Necessary to spin in place rather than the top left corner
            let origin = Nullable(Vector2(float32  secondFlake.Width / 2.0f, float32  secondFlake.Height  / 2.0f ))
         
            let position = Nullable(Vector2(float32 flake.X,float32 flake.Y)) 
           
            let position2 = Nullable(Vector2( float32 secondFlakeValues.X + float32 secondFlake.Width / 2.0f ,
                                              float32 secondFlakeValues.Y + float32 secondFlake.Height  / 2.0f))   
         
            let scale = flake.scale       
            let r = flake.A * Math.PI / 180.0 //used to orientate single F# in random angles.
            let r2 =  60.0 * Math.PI/180.0  
        
            //Yay SPINNERS!
            let layerDepth =  float32 (rnd.Next(0,2) )
            for i in [0.0f .. 5.0f] do  //Lets just place 6 at 60 degree angles to each other, like an F# SNOWFLAKE!!
                    spriteBatch.Draw(textTexture, position=position, rotation=float32 r2 * i +  flake.spinfactor, scale=scale, layerDepth=layerDepth)
            spriteBatch.Draw( secondFlake,position=position2, rotation=float32 secondFlakeValues.spinfactor, origin=origin, scale=secondFlakeValues.scale)
       
      spriteBatch.End()

do
   use game = new Snowflakes()
   game.Run()