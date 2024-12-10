import torch
import torch.nn as nn
import matplotlib.pyplot as plt
import torchvision.transforms as transforms
from PIL import Image

# Bild laden
transform = transforms.Compose([
    transforms.Grayscale(),  # In Graustufen umwandeln
    transforms.ToTensor()    # In Tensor umwandeln
])
image = Image.open('logo_tubaf_2023.png')  # Ersetze durch ein Beispielbild
image_tensor = transform(image).unsqueeze(0)  # Batch-Dimension hinzufügen

# Manuelle Filter definieren
edge_filter = torch.tensor([[[-1, -1, -1],
                             [-1,  8, -1],
                             [-1, -1, -1]]], dtype=torch.float32).unsqueeze(1)

# Convolutional Layer mit fixierten Gewichten
conv = nn.Conv2d(1, 1, kernel_size=3, bias=False)
conv.weight = nn.Parameter(edge_filter, requires_grad=False)

# Filter anwenden
output = conv(image_tensor)

# Ergebnis visualisieren
plt.subplot(1, 2, 1)
plt.title('Original Image')
plt.imshow(image_tensor[0][0], cmap='gray')

plt.subplot(1, 2, 2)
plt.title('Edge Detected')
plt.imshow(output[0][0].detach().numpy(), cmap='gray')
plt.show()
