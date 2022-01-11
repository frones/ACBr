using System;
using System.Drawing.Printing;
using System.IO;
using System.Windows.Forms;
using ACBrLib;
using ACBrLib.Boleto;
using ACBrLib.Core;
using ACBrLib.Core.Boleto;

namespace ACBrLibBoleto.Demo
{
    public partial class FrmMain : Form
    {
        #region Fields

        private ACBrBoleto boleto;

        #endregion Fields

        #region Constructors

        public FrmMain()
        {
            InitializeComponent();

            // Inicializando a dll
            //boleto = new ACBrBoleto();
            boleto = new ACBrBoleto("[Memory]");
        }

        private void FrmMain_FormClosing(object sender, FormClosingEventArgs e)
        {
            // Finalizando a dll
            boleto.Dispose();
            boleto = null;
        }

        private void FrmMain_Shown(object sender, EventArgs e)
        {
            cmbTipoInscricao.EnumDataSource(ACBrPessoa.pJuridica);
            cmbTipoDocumento.EnumDataSource(ACBrTipoDocumento.Tradicional);
            cmbTipoCarteira.EnumDataSource(ACBrTipoCarteira.tctSimples);
            cmbModeloImpressao.EnumDataSource(ACBrBolLayOut.lPadrao);
            cmbBanco.EnumDataSource(ACBrTipoCobranca.cobNenhum);
            cmbRespEmissao.EnumDataSource(ACBrResponEmissao.tbCliEmite);
            cmbLayoutCNAB.EnumDataSource(ACBrLayoutRemessa.c240);

            foreach (string printer in PrinterSettings.InstalledPrinters)
            {
                cmbImpressora.Items.Add(printer);
            }

            if (PrinterSettings.InstalledPrinters.Count > 0)
                cmbImpressora.SelectedIndex = 0;

            // Altera as config de log
            boleto.Config.Principal.LogNivel = NivelLog.logParanoico;

            var logPath = Path.Combine(Application.StartupPath, "Logs");
            if (!Directory.Exists(logPath))
                Directory.CreateDirectory(logPath);

            boleto.Config.Principal.LogPath = logPath;
            boleto.ConfigGravar();

            LoadConfig();
        }

        #endregion Constructors

        #region Methods

        private void LoadConfig()
        {
            boleto.ConfigLer();

            cmbModeloImpressao.SetSelectedValue(boleto.Config.Impressao.Layout);
            chkPreview.Checked = boleto.Config.Impressao.MostrarPreview;
            chkProgresso.Checked = boleto.Config.Impressao.MostrarProgresso;
            chkSetup.Checked = boleto.Config.Impressao.MostrarSetup;
            txtNomeArquivo.Text = boleto.Config.Impressao.NomeArquivo;
            nudCopias.Value = boleto.Config.Impressao.NumeroCopias;
            cmbImpressora.SelectedItem = boleto.Config.Impressao.PrinterName;
            txtDirLogo.Text = boleto.Config.Impressao.DirLogo;
            cmbBanco.SetSelectedValue(boleto.Config.Banco.TipoCobranca);
            txtAgencia.Text = boleto.Config.Cedente.Agencia;
            txtDigAgencia.Text = boleto.Config.Cedente.AgenciaDigito;
            txtDigAgencia.Text = boleto.Config.Cedente.AgenciaDigito;
            txtConta.Text = boleto.Config.Cedente.Conta;
            txtDigConta.Text = boleto.Config.Cedente.ContaDigito;
            txtCodTransmissao.Text = boleto.Config.Cedente.CodigoTransmissao;
            txtConvenio.Text = boleto.Config.Cedente.Convenio;
            txtModalidade.Text = boleto.Config.Cedente.Modalidade;
            txtCodCedente.Text = boleto.Config.Cedente.CodigoCedente;
            cmbRespEmissao.SetSelectedValue(boleto.Config.Cedente.ResponEmissao);
            txtBairro.Text = boleto.Config.Cedente.Bairro;
            txtCEP.Text = boleto.Config.Cedente.CEP;
            txtCidade.Text = boleto.Config.Cedente.Cidade;
            txtCNPJCPF.Text = boleto.Config.Cedente.CNPJCPF;
            txtComplemento.Text = boleto.Config.Cedente.Complemento;
            txtLogradouro.Text = boleto.Config.Cedente.Logradouro;
            txtNomeRes.Text = boleto.Config.Cedente.Nome;
            txtNumeroRes.Text = boleto.Config.Cedente.NumeroRes;
            txtTelefone.Text = boleto.Config.Cedente.Telefone;
            cmbUF.SelectedItem = boleto.Config.Cedente.UF;
            cmbTipoCarteira.SetSelectedValue(boleto.Config.Cedente.TipoCarteira);
            cmbTipoDocumento.SetSelectedValue(boleto.Config.Cedente.TipoDocumento);
            cmbTipoInscricao.SetSelectedValue(boleto.Config.Cedente.TipoInscricao);
            txtDirRemessa.Text = boleto.Config.Diretorio.DirArqRemessa;
            txtDirRetorno.Text = boleto.Config.Diretorio.DirArqRetorno;
            cmbLayoutCNAB.SetSelectedValue(boleto.Config.Diretorio.LayoutRemessa);
            txtNomeRemessa.Text = boleto.Config.Diretorio.NomeArqRemessa;
            txtNomeRetorno.Text = boleto.Config.Diretorio.NomeArqRetorno;
            ckbCedenteRetorno.Checked = boleto.Config.Diretorio.LeCedenteRetorno;
            txtNome.Text = boleto.Config.Email.Nome;
            txtEmail.Text = boleto.Config.Email.Conta;
            txtUsuario.Text = boleto.Config.Email.Usuario;
            txtSenha.Text = boleto.Config.Email.Senha;
            txtHost.Text = boleto.Config.Email.Servidor;
            nudPorta.Text = boleto.Config.Email.Porta;
            ckbSSL.Checked = boleto.Config.Email.SSL;
            ckbTLS.Checked = boleto.Config.Email.TLS;
        }

        private void SaveConfig()
        {
            boleto.Config.Impressao.MostrarPreview = chkPreview.Checked;
            boleto.Config.Impressao.MostrarProgresso = chkProgresso.Checked;
            boleto.Config.Impressao.MostrarSetup = chkSetup.Checked;
            boleto.Config.Impressao.NomeArquivo = txtNomeArquivo.Text;
            boleto.Config.Impressao.NumeroCopias = (int)nudCopias.Value;
            boleto.Config.Impressao.PrinterName = cmbImpressora.Text;
            boleto.Config.Impressao.DirLogo = txtDirLogo.Text;
            boleto.Config.Banco.TipoCobranca = cmbBanco.GetSelectedValue<ACBrTipoCobranca>();
            boleto.Config.Cedente.TipoCarteira = cmbTipoCarteira.GetSelectedValue<ACBrTipoCarteira>();
            boleto.Config.Cedente.TipoDocumento = cmbTipoDocumento.GetSelectedValue<ACBrTipoDocumento>();
            boleto.Config.Cedente.TipoInscricao = cmbTipoInscricao.GetSelectedValue<ACBrPessoa>();
            boleto.Config.Cedente.Agencia = txtAgencia.Text;
            boleto.Config.Cedente.AgenciaDigito = txtDigAgencia.Text;
            boleto.Config.Cedente.Conta = txtConta.Text;
            boleto.Config.Cedente.ContaDigito = txtDigConta.Text;
            boleto.Config.Cedente.CodigoTransmissao = txtCodTransmissao.Text;
            boleto.Config.Cedente.Convenio = txtConvenio.Text;
            boleto.Config.Cedente.Modalidade = txtModalidade.Text;
            boleto.Config.Cedente.CodigoCedente = txtCodCedente.Text;
            boleto.Config.Cedente.ResponEmissao = cmbRespEmissao.GetSelectedValue<ACBrResponEmissao>();
            boleto.Config.Cedente.Bairro = txtBairro.Text;
            boleto.Config.Cedente.CEP = txtCEP.Text;
            boleto.Config.Cedente.Cidade = txtCidade.Text;
            boleto.Config.Cedente.CNPJCPF = txtCNPJCPF.Text;
            boleto.Config.Cedente.Complemento = txtComplemento.Text;
            boleto.Config.Cedente.Logradouro = txtLogradouro.Text;
            boleto.Config.Cedente.Nome = txtNomeRes.Text;
            boleto.Config.Cedente.NumeroRes = txtNumeroRes.Text;
            boleto.Config.Cedente.Telefone = txtTelefone.Text;
            boleto.Config.Cedente.UF = cmbUF.Text;
            boleto.Config.Diretorio.DirArqRemessa = txtDirRemessa.Text;
            boleto.Config.Diretorio.DirArqRetorno = txtDirRetorno.Text;
            boleto.Config.Diretorio.LayoutRemessa = cmbLayoutCNAB.GetSelectedValue<ACBrLayoutRemessa>();
            boleto.Config.Diretorio.NomeArqRemessa = txtNomeRemessa.Text;
            boleto.Config.Diretorio.NomeArqRetorno = txtNomeRetorno.Text;
            boleto.Config.Diretorio.LeCedenteRetorno = ckbCedenteRetorno.Checked;
            boleto.Config.Email.Nome = txtNome.Text;
            boleto.Config.Email.Conta = txtEmail.Text;
            boleto.Config.Email.Usuario = txtUsuario.Text;
            boleto.Config.Email.Senha = txtSenha.Text;
            boleto.Config.Email.Servidor = txtHost.Text;
            boleto.Config.Email.Porta = nudPorta.Text;
            boleto.Config.Email.SSL = ckbSSL.Checked;
            boleto.Config.Email.TLS = ckbTLS.Checked;
            boleto.ConfigGravar();
        }

        #endregion Methods

        #region EventHandlers

        private void BtnDirLogo_Click(object sender, EventArgs e)
        {
            txtDirLogo.Text = Helpers.SelectFolder();
        }

        private void BtnDirRemessa_Click(object sender, EventArgs e)
        {
            txtDirRemessa.Text = Helpers.SelectFolder();
        }

        private void BtnDirRetorno_Click(object sender, EventArgs e)
        {
            txtDirRetorno.Text = Helpers.SelectFolder();
        }

        private void BtnGravarConfig_Click(object sender, EventArgs e)
        {
            SaveConfig();
        }

        private void BtnIncluirTitulo_Click(object sender, EventArgs e)
        {
            var iniPath = Helpers.OpenFile("Titulo (*.ini)|*.ini|Todo os Arquivos (*.*)|*.*");
            if (string.IsNullOrEmpty(iniPath)) return;

            boleto.IncluirTitulos(iniPath);
        }

        private void BtnImprimir_Click(object sender, EventArgs e)
        {
            boleto.Imprimir();
            rtbRespostas.AppendLine("Boletos impressos.");
        }

        private void BtnGerarRemessa_Click(object sender, EventArgs e)
        {
            var ret = "";
            if (string.IsNullOrEmpty(txtDirRemessa.Text))
                ret = Application.StartupPath;
            else
                ret = txtDirRemessa.Text;
            boleto.GerarRemessa(ret, 1, txtNomeRemessa.Text);
            rtbRespostas.AppendLine("Remessa Gerada.");
        }

        private void BtnTotalTitulo_Click(object sender, EventArgs e)
        {
            var ret = boleto.TotalTitulosLista();
            rtbRespostas.AppendLine(ret.ToString());
        }

        private void BtnLimparLista_Click(object sender, EventArgs e)
        {
            boleto.LimparLista();
            rtbRespostas.AppendLine("Lista Limpa.");
        }

        #endregion EventHandlers

        private void BtnConfigDados_Click(object sender, EventArgs e)
        {
            var iniPath = Helpers.OpenFile("Configurar Dados do Cedente (*.ini)|*.ini|Todo os Arquivos (*.*)|*.*");
            if (string.IsNullOrEmpty(iniPath)) return;

            boleto.ConfigurarDados(iniPath);
        }

        private void BtnGerarPDF_Click(object sender, EventArgs e)
        {
            boleto.GerarPDF();
            rtbRespostas.AppendLine("PDF Gerado");
        }

        private void BtnLinhaDigitavel_Click(object sender, EventArgs e)
        {
            var ret = boleto.RetornaLinhaDigitavel(0);
            rtbRespostas.AppendLine(ret);
        }

        private void BtnEnviarEmail_Click(object sender, EventArgs e)
        {
            if (boleto.TotalTitulosLista() == 0) return;

            try
            {
                boleto.EnviarEmail(txtEmail.Text,
                    "Teste envio Boleto",
                    "Boleto em anexo", "");
                rtbRespostas.AppendLine("e-mail enviado!");
            }
            catch (Exception ex)
            {
                rtbRespostas.AppendLine(ex.Message);
            }
        }

        private void BtnListaBancos_Click(object sender, EventArgs e)
        {
            var ret = boleto.ListaBancos();
            rtbRespostas.AppendLine(ret);
        }

        private void BtnCodigoBarras_Click(object sender, EventArgs e)
        {
            var ret = boleto.RetornaCodigoBarras(0);
            rtbRespostas.AppendLine(ret);
        }

        private void BtnLerRetorno_Click(object sender, EventArgs e)
        {
            boleto.LerRetorno(txtDirRetorno.Text, txtNomeRetorno.Text);
            var ret = boleto.ObterRetorno(txtDirRetorno.Text, txtNomeRetorno.Text);
            rtbRespostas.AppendLine(ret.Retorno);
        }

        private void BtnListarOcorrencias_Click(object sender, EventArgs e)
        {
            var ret = boleto.ListaOcorrencias();
            rtbRespostas.AppendLine(ret);
        }

        private void BtnListarOcorrenciasEx_Click(object sender, EventArgs e)
        {
            var ret = boleto.ListaOcorrenciasEX();
            rtbRespostas.AppendLine(ret);
        }

        private void BtnImprimirBoleto_Click(object sender, EventArgs e)
        {
            boleto.Imprimir(0);
            rtbRespostas.AppendLine("Boletos impressos.");
        }

        private void BtnSelecionaBanco_Click(object sender, EventArgs e)
        {
            var codBanco = "001";
            InputBox.Show("Selecionar Banco", "Número do Banco", ref codBanco);

            boleto.SelecionaBanco(codBanco);
        }

        private void BtnGerarHTML_Click(object sender, EventArgs e)
        {
            boleto.GerarHTML();
            rtbRespostas.AppendLine("HTML Gerado");
        }

        private void BtnCaracTitulos_Click(object sender, EventArgs e)
        {
            var ret = boleto.ListaCaractTitulo();
            rtbRespostas.AppendLine(ret);
        }

        private void BtnCodigoMoraAceitos_Click(object sender, EventArgs e)
        {
            var ret = boleto.CodigosMoraAceitos();
            rtbRespostas.AppendLine(ret);
        }

        private void BtnSetDiretorioArquivos_Click(object sender, EventArgs e)
        {
            var sDiretorio = Helpers.SelectFolder();

            boleto.SetDiretorioArquivo(sDiretorio);
            rtbRespostas.AppendLine("Direto setado");
        }

        private void BtnTamNossoNumero_Click(object sender, EventArgs e)
        {
            var sCarteira = "0";
            InputBox.Show("Nosso Numero", "Digite a Carteira", ref sCarteira);

            var sNossoNumero = "0";
            InputBox.Show("Nosso Numero", "Digite a Número", ref sNossoNumero);

            var sConvenio = "0";
            InputBox.Show("Nosso Numero", "Digite o Convenio", ref sConvenio);

            var ret = boleto.TamNossoNumero(sCarteira, sNossoNumero, sConvenio);
            rtbRespostas.AppendLine(ret.ToString());
        }

        private void BtnMontarNossoNumero_Click(object sender, EventArgs e)
        {
            var indice = 0;

            var ret = boleto.MontarNossoNumero(indice);
            rtbRespostas.AppendLine(ret);
        }

        private void BtnEnviarEmailBoleto_Click(object sender, EventArgs e)
        {
            if (boleto.TotalTitulosLista() == 0) return;

            try
            {
                boleto.EnviarEmailBoleto(0, txtEmail.Text,
                    "Teste envio Boleto",
                    "Boleto em anexo", "");
                rtbRespostas.AppendLine("e-mail enviado!");
            }
            catch (Exception ex)
            {
                rtbRespostas.AppendLine(ex.Message);
            }
        }
    }
}