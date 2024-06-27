using System;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.InteropServices;
using System.Text;
using System.Threading.Tasks;

namespace ACBrLib.Boleto
{
    public sealed partial class ACBrBoleto
    {
        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Boleto_Inicializar(string eArqConfig, string eChaveCrypt);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Boleto_Finalizar();

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Boleto_Nome(StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Boleto_Versao(StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Boleto_UltimoRetorno(StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Boleto_ConfigImportar(string eArqConfig);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Boleto_ConfigExportar(StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Boleto_ConfigLer(string eArqConfig);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Boleto_ConfigGravar(string eArqConfig);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Boleto_ConfigLerValor(string eSessao, string eChave, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Boleto_ConfigGravarValor(string eSessao, string eChave, string valor);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Boleto_ConfigurarDados(string eArquivoIni);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Boleto_IncluirTitulos(string eArquivoIni, string eTpSaida);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Boleto_LimparLista();

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Boleto_TotalTitulosLista();

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Boleto_Imprimir(string eNomeImpressora);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Boleto_ImprimirBoleto(int eIndice, string eNomeImpressora);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Boleto_GerarPDF();

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Boleto_SalvarPDF(StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Boleto_GerarPDFBoleto(int eIndice);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Boleto_SalvarPDFBoleto(int eIndice, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Boleto_GerarHTML();

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Boleto_GerarRemessa(string eDir, int eNumArquivo, string eNomeArq);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Boleto_GerarRemessaStream(int eNumArquivo, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Boleto_ObterRetorno(string eDir, string eNomeArq, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Boleto_LerRetorno(string eDir, string eNomeArq);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Boleto_EnviarEmail(string ePara, string eAssunto, string eMensagem, string eCC);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Boleto_EnviarEmailBoleto(int eIndice, string ePara, string eAssunto, string eMensagem, string eCC);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Boleto_SetDiretorioArquivo(string eDir, string eArq);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Boleto_ListaBancos(StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Boleto_ListaCaractTitulo(StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Boleto_ListaOcorrencias(StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Boleto_ListaOcorrenciasEX(StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Boleto_TamNossoNumero(string eCarteira, string enossoNumero, string eConvenio);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Boleto_CodigosMoraAceitos(StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Boleto_SelecionaBanco(string eCodBanco);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Boleto_MontarNossoNumero(int eIndice, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Boleto_RetornaLinhaDigitavel(int eIndice, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Boleto_RetornaCodigoBarras(int eIndice, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Boleto_EnviarBoleto(int eCodigoOperacao, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Boleto_ConsultarTitulosPorPeriodo(string eArquivoIni, StringBuilder buffer, ref int bufferSize);

        protected override void InitializeMethods()
        {
            AddMethod<Boleto_Inicializar>("Boleto_Inicializar");
            AddMethod<Boleto_Finalizar>("Boleto_Finalizar");
            AddMethod<Boleto_Nome>("Boleto_Nome");
            AddMethod<Boleto_Versao>("Boleto_Versao");
            AddMethod<Boleto_UltimoRetorno>("Boleto_UltimoRetorno");
            AddMethod<Boleto_ConfigImportar>("Boleto_ConfigImportar");
            AddMethod<Boleto_ConfigExportar>("Boleto_ConfigExportar");
            AddMethod<Boleto_ConfigLer>("Boleto_ConfigLer");
            AddMethod<Boleto_ConfigGravar>("Boleto_ConfigGravar");
            AddMethod<Boleto_ConfigLerValor>("Boleto_ConfigLerValor");
            AddMethod<Boleto_ConfigGravarValor>("Boleto_ConfigGravarValor");
            AddMethod<Boleto_ConfigurarDados>("Boleto_ConfigurarDados");
            AddMethod<Boleto_IncluirTitulos>("Boleto_IncluirTitulos");
            AddMethod<Boleto_LimparLista>("Boleto_LimparLista");
            AddMethod<Boleto_TotalTitulosLista>("Boleto_TotalTitulosLista");
            AddMethod<Boleto_Imprimir>("Boleto_Imprimir");
            AddMethod<Boleto_ImprimirBoleto>("Boleto_ImprimirBoleto");
            AddMethod<Boleto_GerarPDF>("Boleto_GerarPDF");
            AddMethod<Boleto_SalvarPDF>("Boleto_SalvarPDF");
            AddMethod<Boleto_GerarPDFBoleto>("Boleto_GerarPDFBoleto");
            AddMethod<Boleto_SalvarPDFBoleto>("Boleto_SalvarPDFBoleto");
            AddMethod<Boleto_GerarHTML>("Boleto_GerarHTML");
            AddMethod<Boleto_GerarRemessa>("Boleto_GerarRemessa");
            AddMethod<Boleto_GerarRemessaStream>("Boleto_GerarRemessaStream");
            AddMethod<Boleto_ObterRetorno>("Boleto_ObterRetorno");
            AddMethod<Boleto_LerRetorno>("Boleto_LerRetorno");
            AddMethod<Boleto_EnviarEmail>("Boleto_EnviarEmail");
            AddMethod<Boleto_EnviarEmailBoleto>("Boleto_EnviarEmailBoleto");
            AddMethod<Boleto_SetDiretorioArquivo>("Boleto_SetDiretorioArquivo");
            AddMethod<Boleto_ListaBancos>("Boleto_ListaBancos");
            AddMethod<Boleto_ListaCaractTitulo>("Boleto_ListaCaractTitulo");
            AddMethod<Boleto_ListaOcorrencias>("Boleto_ListaOcorrencias");
            AddMethod<Boleto_ListaOcorrenciasEX>("Boleto_ListaOcorrenciasEX");
            AddMethod<Boleto_TamNossoNumero>("Boleto_TamNossoNumero");
            AddMethod<Boleto_CodigosMoraAceitos>("Boleto_CodigosMoraAceitos");
            AddMethod<Boleto_SelecionaBanco>("Boleto_SelecionaBanco");
            AddMethod<Boleto_MontarNossoNumero>("Boleto_MontarNossoNumero");
            AddMethod<Boleto_RetornaLinhaDigitavel>("Boleto_RetornaLinhaDigitavel");
            AddMethod<Boleto_RetornaCodigoBarras>("Boleto_RetornaCodigoBarras");
            AddMethod<Boleto_EnviarBoleto>("Boleto_EnviarBoleto");
            AddMethod<Boleto_ConsultarTitulosPorPeriodo>("Boleto_ConsultarTitulosPorPeriodo");
        }
    }
}