using System;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.InteropServices;
using System.Text;
using System.Threading.Tasks;

namespace ACBrLib.IBGE
{
    public sealed partial class ACBrIBGE
    {
		
        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int IBGE_Inicializar(string eArqConfig, string eChaveCrypt);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int IBGE_Finalizar();

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int IBGE_Nome(StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int IBGE_Versao(StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int IBGE_UltimoRetorno(StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int IBGE_ConfigImportar(string eArqConfig);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int IBGE_ConfigExportar(StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int IBGE_ConfigLer(string eArqConfig);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int IBGE_ConfigGravar(string eArqConfig);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int IBGE_ConfigLerValor(string eSessao, string eChave, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int IBGE_ConfigGravarValor(string eSessao, string eChave, string valor);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int IBGE_BuscarPorCodigo(int ACodMun, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int IBGE_BuscarPorNome(string eCidade, string eUF, bool Exata, StringBuilder buffer, ref int bufferSize);

        protected override void InitializeMethods()
        {
            AddMethod<IBGE_Inicializar>("IBGE_Inicializar");
            AddMethod<IBGE_Finalizar>("IBGE_Finalizar");
            AddMethod<IBGE_Nome>("IBGE_Nome");
            AddMethod<IBGE_Versao>("IBGE_Versao");
            AddMethod<IBGE_UltimoRetorno>("IBGE_UltimoRetorno");
            AddMethod<IBGE_ConfigImportar>("IBGE_ConfigImportar");
            AddMethod<IBGE_ConfigExportar>("IBGE_ConfigExportar");
            AddMethod<IBGE_ConfigLer>("IBGE_ConfigLer");
            AddMethod<IBGE_ConfigGravar>("IBGE_ConfigGravar");
            AddMethod<IBGE_ConfigLerValor>("IBGE_ConfigLerValor");
            AddMethod<IBGE_ConfigGravarValor>("IBGE_ConfigGravarValor");
            AddMethod<IBGE_BuscarPorCodigo>("IBGE_BuscarPorCodigo");
            AddMethod<IBGE_BuscarPorNome>("IBGE_BuscarPorNome");
        }
    }
}