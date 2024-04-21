import matplotlib.pyplot as plt
import numpy as np

def DFT(x):
    """
    Discrete version of Fourier
    Transform
    """

    N = len(x)
    n = np.arange(N)
    # need cyclic i values to be vertical
    k = n.reshape((N,1))
    e_val = np.exp(-2j * np.pi * k * n / N)
    res = np.dot(e_val,x)

    return res

def IDFT(x):
    """
    Discrete version of Inverse
    Fourier Transform
    """

    N = len(x)
    n = np.arange(N)
    # need cyclic i values to be vertical
    k = n.reshape((N,1))
    e_val = np.exp(2j * np.pi * k * n / N)
    res = 1/N * np.dot(e_val,x)

    return res
  
def FFT(x):
    N = len(x)
    if N <= 1:
        return x
    even = FFT(x[::2])
    odd = FFT(x[1::2])
    factor = np.exp(-2j * np.pi * np.arange(N) / N)
    return np.concatenate([even + factor[:N // 2] * odd, even + factor[N // 2:] * odd])

def IFFT(x):
    N = len(x)
    if N <= 1:
        return x
    even = ifft(x[::2])
    odd = ifft(x[1::2])
    factor = np.exp(2j * np.pi * np.arange(N) / N)
    return np.concatenate([even + factor[:N // 2] * odd, even + factor[N // 2:] * odd]) / 2

# sample  
minval = 0.0
maxval = 1.0
sample = 100
step = maxval / sample
t = np.arange(minval,maxval,step)

# create function
x = np.sin(2*np.pi*2*t)
x += np.sin(2*np.pi*4*t)
x += 2*np.sin(2*np.pi*t)

# forward transform
X = DFT(x)
absX = abs(X)
N = len(X)
n = np.arange(N)
T = N/sample
f = n/T 
halfinterval = N//2
halff = f[:halfinterval]
halfX = absX[:halfinterval]/halfinterval

# reverse transform
new_x = IDFT(X)

plt.figure(figsize = (8, 6))
plt.plot(t, x, 'r')
plt.ylabel('Amplitude')

plt.show()

plt.figure(figsize = (8, 6))
plt.stem(halff, halfX, 'b', markerfmt=" ", basefmt="-b")
plt.xlim(0, 6)
plt.show()

plt.figure(figsize = (8, 6))
plt.plot(t, new_x, 'r')
plt.ylabel('Amplitude')

plt.show()
