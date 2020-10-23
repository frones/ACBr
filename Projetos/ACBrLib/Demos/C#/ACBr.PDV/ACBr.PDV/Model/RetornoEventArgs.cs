using System;

namespace ACBr.PDV.Model
{
    public class RetornoEventArgs : EventArgs
    {
        public string Retorno { get; set; }

        public bool Sucesso { get; set; }
    }
}