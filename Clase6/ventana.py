import tkinter as tk
import subprocess

def enviar_datos():
    # Obtener el dato ingresado en la entrada
    dato = entry.get()
    
    # Ejecutar el programa Fortran y enviar el dato
    resultado = subprocess.run(
        ["./analisis.exe"],  # Ejecutable compilado
        input=dato,  # Enviar el dato como cadena de texto
        stdout=subprocess.PIPE,  # Capturar la salida del programa
        text=True  # Asegurarse de que la salida se maneje como texto
    )

    # Procesar la salida para dividirla en partes
    salida = resultado.stdout.strip()
    partes = salida.split(",")

    if len(partes) == 4:
        # Mostrar los valores en los Labels correspondientes
        label_poblacion.config(text="Población: " + partes[1])
        label_nombre.config(text="Nombre: " + partes[2])
        
        # Mostrar la imagen en un Label
        imagen_path = partes[3].strip()
        imagen = tk.PhotoImage(file=imagen_path)
        label_bandera.config(image=imagen)
        label_bandera.image = imagen  # Guardar referencia para evitar que se borre la imagen

    # Mostrar la salida completa en el área de texto
    output_area.insert(tk.END, salida + '\n')

# Crear la ventana principal
ventana = tk.Tk()
ventana.title("Interfaz Tkinter con Fortran")

# Crear campo de entrada
tk.Label(ventana, text="Ingrese un valor:").pack()
entry = tk.Entry(ventana)
entry.pack()

# Crear botón para enviar el dato
tk.Button(ventana, text="Enviar a Fortran", command=enviar_datos).pack()

# Crear un área de texto para mostrar la salida
output_area = tk.Text(ventana, height=10, width=40)
output_area.pack()

# Crear Labels para mostrar los resultados
label_poblacion = tk.Label(ventana, text="Población: ")
label_poblacion.pack()

label_nombre = tk.Label(ventana, text="Nombre: ")
label_nombre.pack()

label_bandera = tk.Label(ventana)
label_bandera.pack()

# Ejecutar la ventana
ventana.mainloop()
