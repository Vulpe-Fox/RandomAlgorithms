def rgb_to_cmyk(r, g, b):
    if (r, g, b) == (0, 0, 0):
        return 0, 0, 0, 255
    
    c = 1 - r / 255
    m = 1 - g / 255
    y = 1 - b / 255

    # extract key via inverse lerp on white value
    min_val = min(c,m,y)
    k = min_val
    c = (c-min_val) / (1-min_val)
    m = (m-min_val) / (1-min_val)
    y = (y-min_val) / (1-min_val)

    return round(100 * c), round(100 * m), round(100 * y), round(100 * k)

print("insert r, g, b values on three lines:")
input_r = int(input())
input_g = int(input())
input_b = int(input())

c, y, m, k = rgb_to_cmyk(input_r, input_g, input_b)

out = "Resultant cmyk are %s %s %s %s"%(c, y, m, k)

print(out)
