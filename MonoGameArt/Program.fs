open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Input
open System
 
module Text =
   open System.IO
   open System.Drawing
   open System.Windows.Forms
   //Build a new bitmap with it's own F#
   let toBitmapStream text   =
      let maxFont = 15.0f
      use font = new Font(FontFamily.GenericSansSerif, maxFont)
      let size =  TextRenderer.MeasureText(text, font)
      use bitmap = new Bitmap(size.Width , size.Height )
      use g = Graphics.FromImage(bitmap)
      g.SmoothingMode <- Drawing2D.SmoothingMode.AntiAlias
      g.TextRenderingHint <- Text.TextRenderingHint.ClearTypeGridFit
      g.Clear(Color.Transparent)
   //Each new Bitmap has it's own (x,y) so I use this fact to simply add more F#'s to get a quasi 3D effect.
      let cols =  [0 .. 5] |> List.zip [0 .. 5] |> List.iter(fun i -> let  x,y =  i
                                                                      TextRenderer.DrawText(g, text,font, new Point(x,y), Color.White))
  //Save the bitmap to a stream to be used later
      let stream = new System.IO.MemoryStream()
      bitmap.Save(stream, Imaging.ImageFormat.Png)
      stream.Position <- 0L
      stream

let w, h = 1200.0, 800.0
let rnd = System.Random()
let mutable speed = 0.5 
let mutable wind = 45.0
let mutable flakeScarcity = 3 
let GetNextDoubleRnd min max = float32 (rnd.NextDouble() * (max - min) + min)

type flake = { mutable X:float; mutable Y:float; V:float; A:float; scale:Nullable<Vector2>}

type Snowflakes () as this =
   inherit Game ()
   let graphics = new GraphicsDeviceManager(this)
   do graphics.PreferredBackBufferWidth <- 1200
   do graphics.PreferredBackBufferHeight <- 800
   let mutable spriteBatch : SpriteBatch = null
   let mutable textTexture : Texture2D = null
 
   let flakes = ResizeArray<_>()

   override this.LoadContent() =      
      spriteBatch <- new SpriteBatch(this.GraphicsDevice)
      let textStream = Text.toBitmapStream "F#"
      textTexture <- Texture2D.FromStream(this.GraphicsDevice, textStream)
  
   override this.Update(gameTime) =
      //use keys to see a blizzard :)   
      let HandleKeys K =
             match K with
             | Keys.Down -> speed <- speed + 0.1
             | Keys.Up -> if speed >= 0.1 then speed <- speed - 0.1
             | Keys.Left -> wind <- wind + 0.2
             | Keys.Right->  wind <- wind - 0.2 
             | Keys.Space ->  if flakeScarcity >= 1 then flakeScarcity <- flakeScarcity  - 1
             | Keys.F -> if flakeScarcity <= 5 then flakeScarcity <- flakeScarcity  + 1
             | _ -> wind <- wind
 
      if Keyboard.GetState().GetPressedKeys().Length > 0 then HandleKeys (Keyboard.GetState().GetPressedKeys().[0]) |> ignore
      //Alter the scarcity to increase odds of getting more snowflakes
      if (rnd.Next(flakeScarcity)= 0) then
         let x, y = rnd.NextDouble() * w, -16.0
         let a = wind + rnd.NextDouble() * 90.0 //(wind / 2.0)
         let v = (speed + rnd.NextDouble())*2.0
         let sfactor = (GetNextDoubleRnd 0.05 1.0)
        
         let flake = { X=x; Y=y; V=v; A=a;scale=System.Nullable(new Vector2( sfactor, sfactor))}
         flakes.Add(flake)         
      let onScreen, offScreen = flakes |> Seq.toList |> List.partition (fun flake -> flake.Y < h)
      for flake in onScreen do
         let r = flake.A * System.Math.PI / 180.0
         flake.X <- flake.X + ((cos r) * flake.V)
         flake.Y <- flake.Y + ((sin r) * flake.V)        
      for flake in offScreen do
         flakes.Remove(flake) |> ignore

   override this.Draw(gameTime) =
      this.GraphicsDevice.Clear Color.Black
     
      spriteBatch.Begin(SpriteSortMode.Deferred, BlendState.NonPremultiplied)
      let origin = Nullable(Vector2(float32 textTexture.Width/2.0f, float32 textTexture.Height/2.0f))
      
      for flake in flakes do
         let position = Nullable(Vector2(float32 flake.X,float32 flake.Y))  
         let scale = flake.scale       
        // let r = flake.A * Math.PI / 180.0 used to orientate single F# in random angles.
         let r2 = 60.0 * Math.PI/180.0
         for i in [0.0f .. 5.0f] do  //Lets just place 6 at 60 degree angles to each other, like a SNOWFLAKE!!
                spriteBatch.Draw(textTexture, position=position, rotation=float32 r2 * i, scale=scale)
      spriteBatch.End()

do
   use game = new Snowflakes()
   game.Run()