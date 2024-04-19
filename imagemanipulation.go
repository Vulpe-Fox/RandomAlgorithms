import(
	_"image/png"
)

// file manipulation is empty for now, but this is the general setup
// for further manipulation of the tensor.

type ColorTensor struct {
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
func convertToColorTensor(img image.Image) (ColorTensor, error) {
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

	var ct *ColorTensor
	ct = new(ColorTensor)
	ct.colours = colours
	return ct, nil
}

// creates an output image from a colour tensor
func convertToImage(ct ColorTensor) (image.Image, error) {
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

func outputImage(img image.Image, path string) (error) {
	file, err := os.Create(path)
	if err != nil {
		return err
	}
	return nil
}