//http://www.projetoacbr.com.br/forum/topic/25920-classe-para-comunica%C3%A7%C3%A3o-em-c/
//Por: Thiago Raphael Raheem Marinho

using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Net.Sockets;
using System.Net;

namespace NFe
{
	public class Monitor
	{

		public static string ComandoACBR(string comando)
		{
			var retorno = "";

			try
			{
				using (var cliente = new TcpClient())
				{
					String responseData = String.Empty;
					Byte[] data = new Byte[512];

					cliente.Connect("127.0.0.1", 3434);

					if (String.IsNullOrEmpty(comando)) { comando = "NFE.StatusServico"; }

					// concatena na string com o comando os códigos para o monitor entender
					comando += "\r\n.\r\n";

					// abre um stream para comunicação
					NetworkStream stream = cliente.GetStream();

					// lê o que possivelmente estiver sendo enviado pelo monitor
					Int32 bytes = stream.Read(data, 0, data.Length);

					// traduz os bytes em string
					responseData = System.Text.Encoding.ASCII.GetString(data, 0, bytes);


					// Traduz a string para bytes
					data = System.Text.Encoding.ASCII.GetBytes(comando);

					// envia o comando
					stream.Write(data, 0, data.Length);
					stream.Flush();

					int i = -1;
					i = stream.Read(data, 0, 1);
					var letra = "";
					responseData = String.Empty;

					while (letra != ((char)3).ToString())
					{

						i = stream.Read(data, 0, 1);
						letra = System.Text.Encoding.ASCII.GetString(data, 0, i);
						if (letra != ((char)3).ToString())
						{
							responseData += letra;
						}

					}

					retorno = responseData;

					stream.Close();
					cliente.Close();

					return retorno;
				}
			}
			catch (Exception ex)
			{
				retorno += "ERRO: " + ex.Message;

				return retorno;
			}

		}


		/// <summary>
		/// Validar o arquivo da NFe. Arquivo deve estar assinado
		/// </summary>
		/// <param name="path">Caminho completo do arquivo XML gerado</param>
		/// <returns>OK/ERRO -> retorno do monitor</returns>
		public static RetornoComando ValidarNFE(string path)
		{

			var comando = String.Format("NFE.ValidarNFe(\"{0}\")", path);

			var retorno = ComandoACBR(comando);

			return new RetornoComando(retorno);

		}

		public static RetornoComando EnviaNFE(string path, int lote, int assina, int imprime)
		{

			var comando = string.Format("NFE.ENVIARNFE(\"{0}\", {1}, {2}, {3})", path, lote, assina, imprime);

			var retorno = ComandoACBR(comando);

			var retornoComando = new RetornoComando(retorno);

			if (retornoComando.Status)
			{

				if (retornoComando.Resultado.Contains("Rejeicao"))
				{
					retornoComando.Resultado.Replace("OK", "REJEICAO");

				}

			}

			return retornoComando;
		}

		/// <summary>
		/// Imprime a NFE de acordo com os parâmetros informados.
		/// </summary>
		/// <param name="path">Caminho do arquivo XML</param>
		/// <param name="nomeImpressora">Nome da impressora a ser usada [OPCIONAL]</param>
		/// <returns>OK/ERRO -> retorno do monitor</returns>
		public static RetornoComando ImprimirDANFE(string path, string nomeImpressora = "")
		{

			var comando = String.IsNullOrEmpty(nomeImpressora) ?
							 String.Format("NFE.IMPRIMIRDANFE(\"{0}\")", path) :
							 String.Format("NFE.IMPRIMIRDANFE(\"{0}\", \"{1}\")", path, nomeImpressora);

			var retorno = ComandoACBR(comando);

			return new RetornoComando(retorno);
		}

		/// <summary>
		/// Imprime a NFE de acordo com os parâmetros informados.
		/// </summary>
		/// <param name="path">Caminho do arquivo XML</param>
		/// <returns>OK/ERRO -> retorno do monitor</returns>
		public static RetornoComando ImprimirDANFE(string path)
		{

			var comando = String.Format("NFE.IMPRIMIRDANFE(\"{0}\")", path);

			var retorno = ComandoACBR(comando);

			return new RetornoComando(retorno);
		}

		/// <summary>
		/// Cria um arquivo em PDF da NFE de acordo com os parâmetros informados
		/// </summary>
		/// <param name="path">Caminho do arquivo XML</param>
		/// <returns></returns>
		public static RetornoComando ImprimirDANFEPDF(string path)
		{

			var comando = String.Format("NFE.IMPRIMIRDANFEPDF(\"{0}\")", path);

			var retorno = ComandoACBR(comando);

			return new RetornoComando(retorno);

		}

		/// <summary>
		/// Comando que permite baixar NFE (xml).
		/// </summary>
		/// <param name="cnpj"></param>
		/// <param name="chave">Chave de acesso da NF</param>
		/// <returns>XML da NF</returns>
		public static RetornoComando DownloadNFe(string cnpj, string chave)
		{

			var comando = String.Format("NFE.DownloadNFe({0}, {1})", cnpj, chave);

			var retorno = ComandoACBR(comando);

			return new RetornoComando(retorno);

		}

	}

	public class RetornoComando
	{

		public bool Status
		{
			get
			{
				if (!String.IsNullOrEmpty(Resultado) && Resultado.StartsWith("OK"))
				{
					return true;
				}
				else
				{
					return false;
				}
			}
		}
		public string Resultado { get; set; }

		public RetornoComando(string resultado)
		{
			Resultado = resultado;
		}

	}
}
