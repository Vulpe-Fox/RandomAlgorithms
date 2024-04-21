import(
	_"image/png"
)

// file manipulation is empty for now, but this is the general setup
// for further manipulation of the tensor.

type ColourTensor struct {
	colours [][]color.Color
}

func openImg(path string) (image.Image, error) {
	file, err := os.Open(path)
	if err != nil {
		return nil, err
	}
	img, f, err := image.Decode(file)
	if err != nil {
		return nil, err
	}
	if format != "jpeg"{
		return nil, errors.New("not jpeg")
	}
	return img, nil
}

// creates an instance of colour tensor for manipulation from image
func convertToColorTensor(img image.Image) (ColourTensor, error) {
	// get dimensions for array
	dim := img.Bounds().Size()
	dimX := dim.X
	dimY := dim.Y

	var colours [][]color.Color

	for i := 0; i < dimX; i++ {
		var rowCols []color.Color
		for j:=0; j<dimY; j++ {
			rowCols = append(rowCols, img.At(i,j))
		}
		colours = append(colours, rowCols)
	}

	var ct *ColourTensor
	ct = new(ColourTensor)
	ct.colours = colours
	return ct, nil
}

// creates an output image from a colour tensor
func convertToImage(ct ColourTensor) (image.Image, error) {
	colours := ct.colours
	dimX := len(colours)
	dimY := len(colours[0])

	rect := image.Rect(0, 0, dimX, dimY)
	img := image.NewRGBA(rect)

	for i := 0; i < dimX; i++ {
		for j :=0 ; j < dimY; j++ {
			row := colours[i]
			if row == nil {
				img.Set(i, j, (0,0,0,0))
				continue
			}
			pixel := colours[i][j]
			if pixel == nil {
				img.Set(i, j, (0,0,0,0))
				continue
			}
			in := color.RGBAModel.Convert(pixel).(color.RGBA)
			if ok {
				img.Set(i, j, in)
			}
		}
	}

	return img
}

// creates an output image with an image file
func outputImage(img image.Image, path string) (error) {
	file, err := os.Create(path)
	if err != nil {
		return err
	}
	return nil
}

// creates a greyscale copy of image via averaging
func greyscaleByAverage(ct ColourTensor) (ColourTensor) {
	pixels := *ct.colours
	dimX := len(pixels)
	dimY := len(pixels[0])

	// create new tensor object
	newCT := make([][]color.Color, dimX)
	for i := 0; i < len(newCT); i++ {
		newCT[i] = make([]color.Color, dimY)
	}

	// apply function in parallel via wait group
	wg := sync.WaitGroup()
	for i := 0; i < dimX; i++ {
		for j := 0; j < dimY; j++ {
			wg.Add(1)
			go func(i,j int) {
				pixel := pixels[i][j]
				oc, err := color.RGBAModel.Convert(pixel).(color.RGBA)
				if err != nil {
					fmt.Println("Failed to convert pixel %d %d to RGBA", i, j)
				}

				greyscale := uint8((float64(oc.R)+float64(oc.G)+float64(oc.B))/3)

				newCol := color.RGBA(
					greyscale,
					greyscale,
					greyscale,
					oc.A,
				)
				newCT[i][j] = newCol
				wg.Done()
			}(i, j)
		}
	}
	wg.Wait()
	
	var newTensor *ColourTensor
	newTensor = new(ColourTensor)
	newTensor.colours = newCT

	return newTensor
}

// creates greyscale image via weights
func greyscaleByWeights(ct ColourTensor, weightR, weightG, weightB float64) (ColourTensor) {
	pixels := *ct.colours
	dimX := len(pixels)
	dimY := len(pixels[0])

	// create new tensor object
	newCT := make([][]color.Color, dimX)
	for i := 0; i < len(newCT); i++ {
		newCT[i] = make([]color.Color, dimY)
	}

	// apply function in parallel via wait group
	wg := sync.WaitGroup()
	for i := 0; i < dimX; i++ {
		for j := 0; j < dimY; j++ {
			wg.Add(1)
			go func(i,j int) {
				pixel := pixels[i][j]
				oc, err := color.RGBAModel.Convert(pixel).(color.RGBA)
				if err != nil {
					fmt.Println("Failed to convert pixel %d %d to RGBA", i, j)
				}

				greyscale := uint8((weightR*float64(oc.R)+weightG*float64(oc.G)+weightB*float64(oc.B)))

				newCol := color.RGBA(
					greyscale,
					greyscale,
					greyscale,
					oc.A,
				)
				newCT[i][j] = newCol
				wg.Done()
			}(i, j)
		}
	}
	wg.Wait()
	
	var newTensor *ColourTensor
	newTensor = new(ColourTensor)
	newTensor.colours = newCT
	
	return newTensor
}

// creates greyscale image via luminosity curve
func greyscaleByLuminosity(ct ColourTensor) (ColourTensor) {
	wR := 0.21
	wG := 0.72
	wB := 0.07
	return greyscaleByWeights(ct, wR, wG, wB)
}

// desaturates image by a factor of f
func desaturate(ct ColourTensor, f float64) (ColourTensor) {
	pixels := *ct.colours
	dimX := len(pixels)
	dimY := len(pixels[0])

	// create new tensor object
	newCT := make([][]color.Color, dimX)
	for i := 0; i < len(newCT); i++ {
		newCT[i] = make([]color.Color, dimY)
	}

	// apply function in parallel via wait group
	wg := sync.WaitGroup()
	for i := 0; i < dimX; i++ {
		for j := 0; j < dimY; j++ {
			wg.Add(1)
			go func(i,j int) {
				pixel := pixels[i][j]
				oc, err := color.RGBAModel.Convert(pixel).(color.RGBA)
				if err != nil {
					fmt.Println("Failed to convert pixel %d %d to RGBA", i, j)
				}

				greyscale := uint8((0.3*float64(oc.R)+0.6*float64(oc.G)+0.1*float64(oc.B)))

				// linear interpolation of image and greyscale by factor
				newCol := color.RGBA(
					oc.R + f*(greyscale - oc.R),
					oc.G + f*(greyscale - oc.G),
					oc.B + f*(greyscale - oc.B),
					oc.A,
				)
				newCT[i][j] = newCol
				wg.Done()
			}(i, j)
		}
	}
	wg.Wait()
	
	var newTensor *ColourTensor
	newTensor = new(ColourTensor)
	newTensor.colours = newCT
	
	return newTensor
}

// creates a copy of an image in greyscale via complete desaturation
func greyscaleByDesaturation(ct ColourTensor) (ColourTensor) {
	f := 1
	return desaturate(ct, f)
}
