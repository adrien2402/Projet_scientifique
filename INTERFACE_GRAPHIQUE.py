# Importation des modules nécessaires
import tkinter as tk  # Module pour créer des interfaces graphiques
import tkinter.messagebox  # Module pour afficher des boîtes de dialogue
# vérifier que customtkinter est installé sur l'ordinateur sinon entrer la commande pip install customtkinter
import customtkinter  # Bibliothèque pour des widgets Tkinter personnalisés 
import subprocess  # Module pour exécuter des sous-processus
import numpy as np  # Bibliothèque pour le calcul scientifique
import matplotlib.pyplot as plt  # Bibliothèque pour les tracés
from matplotlib.backends.backend_tkagg import FigureCanvasTkAgg  # Intégration de Matplotlib avec Tkinter
from PIL import Image, ImageTk  # Bibliothèque pour la manipulation d'images
import threading  # Module pour gérer les threads

# Configuration du mode d'apparence de customtkinter
customtkinter.set_appearance_mode("System")
customtkinter.set_default_color_theme("blue")

# Définition de la classe principale de l'application
class App(customtkinter.CTk):
    def __init__(self):
        super().__init__()

        # Configuration de la fenêtre principale
        self.title("INTERFACE GRAPHIQUE")
        self.geometry(f"{1920}x1080")

        # Configuration de la grille
        self.grid_columnconfigure(0, weight=0)
        self.grid_columnconfigure(1, weight=1)
        self.grid_rowconfigure(0, weight=1)

        # Création et configuration de la barre latérale
        self.sidebar_frame = customtkinter.CTkFrame(self, width=180, corner_radius=0)
        self.sidebar_frame.grid(row=0, column=0, sticky="nsew")
        self.sidebar_frame.grid_propagate(False)
        self.sidebar_frame.grid_rowconfigure(4, weight=1)
        
        # Ajout d'un label pour le titre 
        self.logo_label = customtkinter.CTkLabel(self.sidebar_frame, text="PROJET", font=customtkinter.CTkFont(size=20, weight="bold"))
        self.logo_label.grid(row=0, column=0, padx=20, pady=(20, 10))
        
        # Ajout des boutons de la barre latérale
        self.sidebar_button_1 = customtkinter.CTkButton(self.sidebar_frame, command=self.compile_fortran, text="COMPILATION")
        self.sidebar_button_1.grid(row=1, column=0, padx=20, pady=10)
        self.sidebar_button_2 = customtkinter.CTkButton(self.sidebar_frame, command=self.execute_program, text="EXECUTION")
        self.sidebar_button_2.grid(row=2, column=0, padx=20, pady=10)
        self.sidebar_button_3 = customtkinter.CTkButton(self.sidebar_frame, command=self.plot_from_tecplot, text="GRAPHIQUE")
        self.sidebar_button_3.grid(row=3, column=0, padx=20, pady=10)
        
        # Ajout des options pour le mode d'apparence
        self.appearance_mode_label = customtkinter.CTkLabel(self.sidebar_frame, text="MODE :", anchor="w")
        self.appearance_mode_label.grid(row=6, column=0, padx=20, pady=(10, 0))
        self.appearance_mode_optionemenu = customtkinter.CTkOptionMenu(self.sidebar_frame, values=["Light", "Dark"], command=self.change_appearance_mode_event)
        self.appearance_mode_optionemenu.grid(row=7, column=0, padx=20, pady=(10, 10))

        # Création et configuration du cadre de paramètres
        self.settings_frame = customtkinter.CTkFrame(self, width=200)
        self.settings_frame.grid(row=0, column=1, padx=(20, 1400), pady=(20, 20), sticky="nsew")

        # Dictionnaire des paramètres avec leurs valeurs par défaut
        self.settings_entries = {
            "Nx": 51,
            "Ny": 51,
            "CFL": 1.0,
            "Fo": 0.1,
            "tf": 10.0,
            "L": 1.0,
            "Vmax": 1.0,
            "pini": 0.0,
            "rtol": 0.01,
            "freq": 1000,
            "Re": 100.0,
        }

        # Ajout des champs d'entrée pour les paramètres
        row_counter = 0
        self.entries = {}
        for label_text, default_value in self.settings_entries.items():
            label = customtkinter.CTkLabel(self.settings_frame, text=label_text, anchor="w")
            label.grid(row=row_counter, column=0, padx=(20, 10), pady=10)
            entry = customtkinter.CTkEntry(self.settings_frame)
            entry.insert(0, str(default_value))
            entry.grid(row=row_counter, column=1, padx=(0, 20), pady=10)
            self.entries[label_text] = entry
            row_counter += 1

        # Ajout des boutons radio pour les options de choix
        self.choice_var = tk.StringVar()
        self.choice_var.set("CDS4")
        self.choices = ["CDS4", "CDS2", "UPWIND"]
        for choice in self.choices:
            radio_button = customtkinter.CTkRadioButton(self.settings_frame, text=choice, variable=self.choice_var, value=choice)
            radio_button.grid(row=row_counter, column=1, padx=(20, 20), pady=10)
            row_counter += 1

        # Ajout d'un bouton pour sauvegarder les paramètres
        self.save_button = customtkinter.CTkButton(self, text="SAUVEGARDE", command=self.save_data)
        self.save_button.grid(row=0, column=1, padx=(80, 1450), pady=(950, 30), sticky="e")

        # Création et configuration du cadre pour le graphique
        self.graph_frame = customtkinter.CTkFrame(self)
        self.graph_frame.grid(row=0, column=1, padx=(300, 20), pady=20, sticky="nsew")
        self.grid_columnconfigure(1, weight=1)

        # Variable pour stocker le widget canvas de Matplotlib
        self.canvas = None

        # Ajout d'un label pour la progression
        self.progression_label = customtkinter.CTkLabel(self.sidebar_frame, text="Progression", font=customtkinter.CTkFont(size=16, weight="bold"))
        self.progression_label.grid(row=4, column=0, padx=2)

        # Ajout d'un widget Text pour afficher la progression
        self.terminal_output_text = tk.Text(self.sidebar_frame, height=2,width = 10, state='disabled')
        self.terminal_output_text.grid(row=5, column=0, padx=20, pady= (20,600))


    # Fonction pour sauvegarder les paramètres dans un fichier
    def save_data(self):
        data = {}
        for label_text, entry_widget in self.entries.items():
            data[label_text] = entry_widget.get()

        choice = self.choice_var.get()

        with open("input.dat", "w") as file:
            for key, value in data.items():
                file.write(f"{key} = {value}\n")
            file.write(f"Option_Selection = {choice}")

        tkinter.messagebox.showinfo("Succès", "Informations enregistrées avec succès")

    # Fonction pour changer le mode d'apparence
    def change_appearance_mode_event(self, new_appearance_mode: str):
        customtkinter.set_appearance_mode(new_appearance_mode)

    # Fonction pour compiler un fichier Fortran
    def compile_fortran(self):
        fortran_file = "optiongraphique.f95"
        try:
            subprocess.run(["gfortran", fortran_file, "-o", "etape7"])
            tkinter.messagebox.showinfo("Compilation réussie", "Le fichier Fortran a été compilé avec succès.")
        except Exception as e:
            tkinter.messagebox.showerror("Erreur de compilation", f"Erreur lors de la compilation du fichier Fortran : {e}")

    def execute_program(self):
        def run_program():
            try:
                process = subprocess.Popen(["./etape7"], stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)
                while True:
                    output = process.stdout.readline()
                    if output == "" and process.poll() is not None:
                        break
                    if output:
                        self.terminal_output_text.config(state='normal')
                        self.terminal_output_text.insert(tk.END, output)
                        self.terminal_output_text.see(tk.END)
                        self.terminal_output_text.config(state='disabled')
                rc = process.poll()
                if rc == 0:
                    tkinter.messagebox.showinfo("Exécution réussie", "Le programme a été exécuté avec succès.")
                else:
                    tkinter.messagebox.showerror("Erreur d'exécution", f"Erreur lors de l'exécution du programme.")
            except Exception as e:
                tkinter.messagebox.showerror("Erreur d'exécution", f"Erreur lors de l'exécution du programme : {e}")

        threading.Thread(target=run_program).start()


    # Fonction pour lire les résultats du fichier de sortie
    def read_output_file(self):
        try:
            with open("output.txt", "r") as file:
                lines = file.readlines()
                nbr_iterations = int(lines[0].strip())
                time = float(lines[1].strip())
                tkinter.messagebox.showinfo("Résultats", f"Nombre d'itérations : {nbr_iterations}\nTemps d'exécution : {time} secondes")
                return nbr_iterations
        except FileNotFoundError:
            tkinter.messagebox.showerror("Erreur", "Le fichier output.txt n'a pas été trouvé.")
            return None
        except Exception as e:
            tkinter.messagebox.showerror("Erreur", f"Erreur lors de la lecture du fichier output.txt : {e}")
            return None

    # Fonction pour tracer les données à partir des fichiers Tecplot
    def plot_from_tecplot(self):
        # Efface les graphiques existants
        if self.canvas is not None:
            self.canvas.get_tk_widget().destroy()

        # Appel à la méthode pour récupérer le nombre d'itérations
        nbr_iterations = self.read_output_file()

        # Vérification si le nombre d'itérations a été récupéré
        if nbr_iterations is None:
            tkinter.messagebox.showerror("Erreur", "Nombre d'itérations non trouvé.")
            return
        # Récupération du fichier de la dernière itération 
        filename = "resultat/" + self.choice_var.get() + "/ite" + str(nbr_iterations) + ".tec"
        with open(filename, "r") as file:
            for _ in range(3):
                next(file)
            data = np.loadtxt(file)

        x_data = data[:, 0]
        y_data = data[:, 1]
        u_data = data[:, 2]
        v_data = data[:, 3]
        p_data = data[:, 4]

        # Création des grilles et des tracés
        X, Y = np.meshgrid(np.unique(x_data), np.unique(y_data))
        U = u_data.reshape((len(np.unique(y_data)), len(np.unique(x_data))))
        V = v_data.reshape((len(np.unique(y_data)), len(np.unique(x_data))))
        P = p_data.reshape((len(np.unique(y_data)), len(np.unique(x_data))))
        fig, axes = plt.subplots(2, 2, figsize=(8, 8))

        # Tracé de la variable U
        im = axes[0, 0].contourf(X, Y, U, cmap='viridis')
        axes[0, 0].set_xlabel('X')
        axes[0, 0].set_ylabel('Y')
        axes[0, 0].set_title('U')
        axes[0, 0].grid(True)
        plt.colorbar(im, ax=axes[0, 0], label='u')

        # Tracé de la variable V
        iv = axes[0, 1].contourf(X, Y, V, cmap='plasma')
        axes[0, 1].set_xlabel('X')
        axes[0, 1].set_ylabel('Y')
        axes[0, 1].set_title('V')
        axes[0, 1].grid(True)
        plt.colorbar(iv, ax=axes[0, 1], label='v')

        # Tracé de la variable P
        ip = axes[1, 0].contourf(X, Y, P, cmap='jet')
        axes[1, 0].set_xlabel('X')
        axes[1, 0].set_ylabel('Y')
        axes[1, 0].set_title('P')
        axes[1, 0].grid(True)
        plt.colorbar(ip, ax=axes[1, 0], label='p')

        # Tracé de la norme de la vitesse
        inorme = axes[1, 1].contourf(X, Y, np.sqrt(U**2 + V**2), cmap='inferno')
        axes[1, 1].set_xlabel('X')
        axes[1, 1].set_ylabel('Y')
        axes[1, 1].set_title('Norme vitesse')
        axes[1, 1].grid(True)
        plt.colorbar(inorme, ax=axes[1, 1], label='Norme vitesse')

        # Ajustement de la disposition des sous-graphiques
        fig.tight_layout()

        # Intégration du graphique dans l'interface Tkinter
        self.canvas = FigureCanvasTkAgg(fig, master=self.graph_frame)
        self.canvas.draw()
        self.canvas.get_tk_widget().pack(side=tk.TOP, fill=tk.BOTH, expand=1)

# Exécution de l'application
if __name__ == "__main__":
    app = App()
    app.mainloop()
