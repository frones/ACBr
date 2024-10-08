import ctypes
import json
import os
import sys
from ctypes import CDLL, POINTER, byref, c_bool, c_char_p, c_int, create_string_buffer, c_ulong, c_char

# Obtem a pasta do projeto
diretorio_script = os.path.dirname(os.path.abspath(__file__))

#Constantes de Configuração
#DLL ACBrLibSAT utilizada neste projeto é 64 ST (Single Thread)
PATH_DLL                = os.path.abspath(os.path.join(diretorio_script,r"ACBrMail64.dll"))
PATH_ACBRLIB            = os.path.abspath(os.path.join(diretorio_script, "ACBrLib.INI"))
PATH_LOG                = os.path.abspath(os.path.join(diretorio_script, "Log"))

#Cria a pasta log se nao existir
if not os.path.exists(PATH_LOG):
   os.makedirs(PATH_LOG) 

#Verifica se a dll está no path indicado
if not os.path.exists(PATH_DLL):
   print("-------------------------------------------")
   print(f"O arquivo '{PATH_DLL}' não existe.")
   print("Verifique se a dll está no caminho indicado")
   print("-------------------------------------------")
   sys.exit(1)

#Validar se json 
def validar_json(json_str):
    try:
        json.loads(json_str)
        return True
    except json.decoder.JSONDecodeError:
        return False 

# Carregar a DLL, ajustes os paths para seu ambiente.
acbr_lib = ctypes.CDLL(PATH_DLL)


# Inicializa Lib 
resposta = acbr_lib.MAIL_Inicializar(PATH_ACBRLIB.encode("utf-8"),"".encode("utf-8"))
if resposta != 0:
    print('MAIL_Inicializar | Erro Código: ',resposta)
    sys.exit(1)

#configurando tipo de resposta retorno 
acbr_lib.MAIL_ConfigGravarValor("Principal".encode("utf-8"), "TipoResposta".encode("utf-8"), str(2).encode("utf-8"))

#Configurando o log da Biblioteca
acbr_lib.MAIL_ConfigGravarValor("Principal".encode("utf-8"), "LogNivel".encode("utf-8"), str(4).encode("utf-8"))
acbr_lib.MAIL_ConfigGravarValor("Principal".encode("utf-8"), "LogPath".encode("utf-8"), PATH_LOG.encode("utf-8"))

#Configurando o login do Email de Envio
acbr_lib.MAIL_ConfigGravarValor("Email".encode("utf-8"), "Nome".encode("utf-8"), "Projeto ACBR".encode("utf-8"))
acbr_lib.MAIL_ConfigGravarValor("Email".encode("utf-8"), "Servidor".encode("utf-8"), "smtp.gmail.com".encode("utf-8"))
acbr_lib.MAIL_ConfigGravarValor("Email".encode("utf-8"), "Conta".encode("utf-8"), "xxxxxxx@gmail.com".encode("utf-8"))
acbr_lib.MAIL_ConfigGravarValor("Email".encode("utf-8"), "Usuario".encode("utf-8"), "xxxxxx@gmail.com".encode("utf-8"))
acbr_lib.MAIL_ConfigGravarValor("Email".encode("utf-8"), "Senha".encode("utf-8"), "suaSenha".encode("utf-8"))
acbr_lib.MAIL_ConfigGravarValor("Email".encode("utf-8"), "SSL".encode("utf-8"), "0".encode("utf-8"))
acbr_lib.MAIL_ConfigGravarValor("Email".encode("utf-8"), "TLS".encode("utf-8"), "1".encode("utf-8"))
acbr_lib.MAIL_ConfigGravarValor("Email".encode("utf-8"), "Porta".encode("utf-8"), "587".encode("utf-8"))
acbr_lib.MAIL_ConfigGravarValor("Email".encode("utf-8"), "SSLType".encode("utf-8"), "5".encode("utf-8"))

#Limpando lista de emails
resultado = acbr_lib.MAIL_Clear()
if resposta != 0:
    print('MAIL_Clear | Erro ', resultado)

#Assunto 
resultado = acbr_lib.MAIL_SetSubject( 'Teste de email'.encode("utf-8"))
if resposta != 0:
    print('MAIL_SetSubject |Erro código',resultado) 

#Endereco Destinatario
resultado = acbr_lib.MAIL_AddAddress( "email@destinatario.com.br".encode("utf-8"))
if resposta != 0:
    print('MAIL_AddAddress |Erro código',resultado) 

#Corpo Email        
resultado = acbr_lib.MAIL_AddBody('TESTE DE ENVIO MT'.encode("utf-8"));
if resposta != 0:
    print('MAIL_AddBody |Erro código',resultado)    
    
#Corpo Alternativo Email 
resultado = acbr_lib.MAIL_AddAltBody('TESTE DE ENVIO ST Python '.encode("utf-8"));
if resposta != 0:
    print('MAIL_AddBodyAlter |Erro código',resultado)     

#Enviar Email        
resultado = acbr_lib.MAIL_Send( 1)
if resultado == 0:
    print('MAIL Send_OK |',resultado)    
else:
    print('MAIL Send_ |Erro código',resultado)    
    
print('Obrigado por executar o teste :)')
acbr_lib.MAIL_Finalizar()