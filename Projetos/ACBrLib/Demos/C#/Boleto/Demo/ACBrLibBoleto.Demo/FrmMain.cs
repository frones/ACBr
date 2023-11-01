using System;
using System.Drawing.Printing;
using System.IO;
using System.Windows.Forms;
using ACBrLib;
using ACBrLib.Boleto;
using ACBrLib.Core;
using ACBrLib.Core.Boleto;
using ACBrLib.Core.DFe;

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
            boleto = new ACBrBoleto();
            //boleto = new ACBrBoleto("[Memory]"); -- Exemplo utilizar ACBrlib.ini em memória.
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
            cmbSSlType.EnumDataSource(SSLType.LT_TLSv1_2);
            cmbHttp.EnumDataSource(SSLHttpLib.httpOpenSSL);
            cmbOperacao.EnumDataSource(OperacaoBoleto.tpInclui);

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

            txtClientID.Text = boleto.Config.CedenteWebservice.ClientID;
            txtClientSecret.Text = boleto.Config.CedenteWebservice.ClientSecret;
            txtKeyUser.Text = boleto.Config.CedenteWebservice.KeyUser;
            txtScope.Text = boleto.Config.CedenteWebservice.Scope;
            chkIndicadorPix.Checked = boleto.Config.CedenteWebservice.IndicadorPix;

            chkGravarLog.Checked = boleto.Config.Webservice.LogRegistro;
            txtPathLog.Text = boleto.Config.Webservice.PathGravarRegistro;

            var ambiente = boleto.Config.Webservice.Ambiente;
            rdbProducao.Checked = ambiente == AmbienteWebservice.Homologaçao;
            rdbHomologacao.Checked = ambiente == AmbienteWebservice.Producao;

            cmbOperacao.SetSelectedValue(boleto.Config.Webservice.Operacao);
            cmbSSlType.SetSelectedValue(boleto.Config.Webservice.SSLType);
            cmbHttp.SetSelectedValue(boleto.Config.DFe.SSLHttpLib);
            txtVersao.Text = boleto.Config.Webservice.VersaoDF;
            nudTimeOut.Value = boleto.Config.Webservice.Timeout;
            txtArquivoCRT.Text = boleto.Config.Webservice.ArquivoCRT;
            txtArquivoKEY.Text = boleto.Config.Webservice.ArquivoKEY;

        }

        private void SaveConfig()
        {
            //Salvar primeiro Tipo de Inscrição depois CNPJ ou CPF.
            boleto.Config.Cedente.TipoInscricao = cmbTipoInscricao.GetSelectedValue<ACBrPessoa>();
            boleto.Config.Cedente.CNPJCPF = txtCNPJCPF.Text;
            //

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

            boleto.Config.CedenteWebservice.ClientID = txtClientID.Text;
            boleto.Config.CedenteWebservice.ClientSecret = txtClientSecret.Text;
            boleto.Config.CedenteWebservice.KeyUser = txtKeyUser.Text;
            boleto.Config.CedenteWebservice.Scope = txtScope.Text;
            boleto.Config.CedenteWebservice.IndicadorPix = chkIndicadorPix.Checked;

            boleto.Config.Webservice.LogRegistro = chkGravarLog.Checked;
            boleto.Config.Webservice.PathGravarRegistro = txtPathLog.Text;
            boleto.Config.Webservice.Ambiente = rdbProducao.Checked ? AmbienteWebservice.Homologaçao : AmbienteWebservice.Producao;
            boleto.Config.Webservice.Operacao = cmbOperacao.GetSelectedValue<OperacaoBoleto>();
            boleto.Config.Webservice.SSLType = cmbSSlType.GetSelectedValue<SSLType>();
            boleto.Config.DFe.SSLHttpLib = cmbHttp.GetSelectedValue<SSLHttpLib>();
            boleto.Config.Webservice.VersaoDF = txtVersao.Text;
            boleto.Config.Webservice.Timeout = (int)nudTimeOut.Value;
            boleto.Config.Webservice.ArquivoCRT = txtArquivoCRT.Text;
            boleto.Config.Webservice.ArquivoKEY = txtArquivoKEY.Text;

            boleto.ConfigGravar();
        }

        private void ConfigBoleto()
        {
            /*
            * Exemplo de uso da classe Banco / Conta / Cedente para geração dos dados do Boleto  
            * Preenchimento com os dados mínimos para geração de um Boleto
            * Descomente as demais classes que precisar usar
            */

            var banco = new Banco();
            banco.TipoCobranca = ACBrTipoCobranca.cobBancoDoBrasil;
            banco.CNAB = ACBrLayoutRemessa.c240;
            //banco.VersaoArquivo = 0;
            //banco.VersaoLote = 0;

            var conta = new Conta();
            conta.Agencia = "1111";
            conta.DigitoAgencia = "1";
            conta.Numero = "99999";
            conta.Digito = "9";

            var cedente = new Cedente();
            cedente.Nome = "Projeto ACBr";
            cedente.CNPJCPF = "18760540000139";
            cedente.Logradouro = "Rua Cel Aureliano Camargo ";
            cedente.Numero = "973";
            cedente.CEP = "18280000";
            cedente.Cidade = "Tatui";
            cedente.UF = "SP";
            cedente.Complemento = "casa";
            cedente.RespEmis = ACBrResponEmissao.tbCliEmite;
            cedente.CodigoCedente = "123456";
            cedente.LayoutBol = ACBrBolLayOut.lPadrao;
            cedente.CaracTitulo = CaracTitulo.tcSimples;
            cedente.TipoCarteira = ACBrTipoCarteira.tctRegistrada;
            cedente.TipoDocumento = ACBrTipoDocumento.Tradicional;
            cedente.Modalidade = "17";
            //cedente.CodigoTransmissao = "";
            cedente.Convenio = "123456";

            BoletoInfo[] boletoInfo = new BoletoInfo[3];
            boletoInfo[0] = banco;
            boletoInfo[1] = conta;
            boletoInfo[2] = cedente;

            boleto.ConfigurarDados(boletoInfo);

        }

        private void GerarTitulo()
        {
            /*
            * Exemplo de uso da classe Titulo para geração dos Titulos em ACBrBoleto  
            * Preenchimento com os dados mínimos para geração de um Boleto
            * Descomente as demais classes que precisar usar
            */

            Titulo[] titulo = new Titulo[1];
            titulo[0] = new Titulo();

            titulo[0].NumeroDocumento = "000001";
            titulo[0].NossoNumero = "12345";
            titulo[0].Carteira = "17";
            titulo[0].ValorDocumento = 100.00M;
            titulo[0].Vencimento = DateTime.Now.AddDays(30);
            titulo[0].DataDocumento = DateTime.Now;
            titulo[0].DataProcessamento = DateTime.Now;
            //titulo[0].DataAbatimento = DateTime.Now;
            //titulo[0].ValorAbatimento = 0;
            titulo[0].DataDesconto = DateTime.Now.AddDays(20);
            titulo[0].TipoDesconto = TipoDesconto.tdNaoConcederDesconto;
            titulo[0].ValorDesconto = 0.5M;
            titulo[0].CodigoMora = "1";
            titulo[0].ValorMoraJuros = 0.2M;
            titulo[0].DataMoraJuros = DateTime.Now.AddDays(30);
            titulo[0].ValorIOF = 0;
            titulo[0].ValorOutrasDespesas = 2.50M;
            titulo[0].DataMulta = DateTime.Now.AddDays(30);
            titulo[0].MultaValorFixo = true;
            titulo[0].PercentualMulta = 5.00M;
            titulo[0].DiasDeProtesto = 0;
            titulo[0].DataProtesto = DateTime.Now.AddDays(60);
            titulo[0].TipoDiasProtesto = TipoDiasIntrucao.diCorridos;
            //titulo[0].DiasDeNegativacao = 0;
            //titulo[0].DataNegativacao = DateTime.Now.AddDays(90);
            titulo[0].CodigoNegativacao = CodigoNegativacao.cnNenhum;
            titulo[0].TipoDiasNegativacao = TipoDiasIntrucao.diCorridos;
            //titulo[0].DataBaixa = DateTime.Now.AddDays(30);
            //titulo[0].DataLimitePagto = DateTime.Now.AddDays(30);
            titulo[0].Especie = "DM";
            titulo[0].EspecieMod = "R$";

            titulo[0].Sacado.NomeSacado = "José da Silva";
            titulo[0].Sacado.Pessoa = ACBrPessoa.pFisica;
            titulo[0].Sacado.CNPJCPF = "99999999999";
            titulo[0].Sacado.Logradouro = "Rua da Colina";
            titulo[0].Sacado.Numero = "1111";
            titulo[0].Sacado.Bairro = "Centro";
            titulo[0].Sacado.Complemento = "Prédio 2";
            titulo[0].Sacado.Cidade = "Tatui";
            titulo[0].Sacado.UF = "SP";
            titulo[0].Sacado.CEP = "18280-000";
            titulo[0].Sacado.Email = "josesilva@mail.com";

            titulo[0].Sacado.Avalista.NomeAvalista = "Sociedade Consultoria";
            titulo[0].Sacado.Avalista.Pessoa = ACBrPessoa.pJuridica;
            titulo[0].Sacado.Avalista.CNPJCPF = "99999999999999";
            titulo[0].Sacado.Avalista.Logradouro = "Rua Frei Caneca";
            titulo[0].Sacado.Avalista.Numero = "100";
            titulo[0].Sacado.Avalista.Complemento = "Predio 2";
            titulo[0].Sacado.Avalista.Bairro = "Centro";
            titulo[0].Sacado.Avalista.Cidade = "Sao Paulo";
            titulo[0].Sacado.Avalista.UF = "SP";
            titulo[0].Sacado.Avalista.CEP = "18280000";
            titulo[0].Sacado.Avalista.Email = "sconsultoria@mail.com";
            titulo[0].Sacado.Avalista.InscricaoNr = "99999999999";

            //titulo[0].Mensagem.Add("Mensagem linha 1");
            //titulo[0].Mensagem.Add("Mensagem Linha 2");
            //titulo[0].Informativo.Add("Informativo Boleto");
            //titulo[0].Detalhamento.Add("Detalhamento de Fatura 1");        

            titulo[0].Instrucao1 = "10";
            titulo[0].Instrucao2 = "11";

            titulo[0].Aceite = AceiteTitulo.atSim;
            titulo[0].OcorrenciaOriginal.Tipo = TipoOcorrencia.toRemessaRegistrar;

            titulo[0].SeuNumero = "000001";
            titulo[0].TipoImpressao = TipoImpressao.tipNormal;
            titulo[0].CarteiraEnvio = CarteiraEnvio.tceCedente;

            //titulo[0].Competencia = "";
            //titulo[0].ArquivoLogoEmp = "";
            //titulo[0].Verso = false;
            //titulo[0].Parcela = 1;
            //titulo[0].TotalParcelas = 1;

            /*var nfe1 = new BoletoNotaFiscal();
            nfe1.ChaveNFe = "12345678901234567890123456789012345678901234";
            nfe1.EmissaoNFe = DateTime.Now;
            nfe1.NumNFe = "10001";
            nfe1.ValorNFe = 100.00M;
            titulo[0].NotaFiscais.Add(nfe1); */

            boleto.IncluirTitulos(titulo);

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

        private async void btnSalvarPDF_Click(object sender, EventArgs e)
        {
            var nomeArquivo = Helpers.SelectFolder() + $@"\{Guid.NewGuid()}.pdf";
            try
            {

                using (FileStream aStream = File.Create(nomeArquivo))
                {
                    boleto.GerarPDF(aStream);
                    byte[] buffer = new Byte[aStream.Length];
                    await aStream.ReadAsync(buffer, 0, buffer.Length);
                    await aStream.FlushAsync();
                    aStream.Seek(0, SeekOrigin.End);
                    await aStream.WriteAsync(buffer, 0, buffer.Length);
                }

                rtbRespostas.AppendLine($"PDF Salvo em: {nomeArquivo}");
            }
            catch (Exception ex)
            {
                rtbRespostas.AppendLine(ex.Message);
            }
        }

        private void btnClasseTitulo_Click(object sender, EventArgs e)
        {

            ConfigBoleto();            
            GerarTitulo();
            rtbRespostas.AppendLine("Título(s) adicionado(s)." );
        }

        private void btnEnviarBoletoWebService_Click(object sender, EventArgs e)
        {
            try
            {
                var ret = boleto.EnviarBoleto(0);
                rtbRespostas.AppendLine(ret.Retorno);
            } 
            catch (Exception ex)
            {
                rtbRespostas.AppendLine(ex.Message);
            }
        }

        private void btnCarregarConfiguracoes_Click(object sender, EventArgs e)
        {
            LoadConfig();
        }

        private void btnPathLog_Click(object sender, EventArgs e)
        {
            txtPathLog.Text = Helpers.SelectFolder();
        }

        private void btnArquivoCRT_Click(object sender, EventArgs e)
        {
            txtArquivoCRT.Text = Helpers.OpenFile("Arquivos CRT (*.crt)|*.crt|Todos os Arquivos (*.*)|*.*");
        }

        private void btnArquivoKEY_Click(object sender, EventArgs e)
        {
            txtArquivoKEY.Text = Helpers.OpenFile("Arquivos KEY (*.key)|*.key|Todos os Arquivos (*.*)|*.*");
        }

        private void btnConsulaLista_Click(object sender, EventArgs e)
        {
            var iniPath = Helpers.OpenFile("Consulta (*.ini)|*.ini|Todo os Arquivos (*.*)|*.*");
            if (string.IsNullOrEmpty(iniPath)) return;

            var ret = boleto.ConsultarTitulosPorPeriodo(iniPath);

            rtbRespostas.AppendLine(ret);
        }

        private void btnBaixaTitulo_Click(object sender, EventArgs e)
        {
            try
            {
                var ret = boleto.EnviarBoleto(OperacaoBoleto.tpBaixa);
                rtbRespostas.AppendLine(ret.Retorno);
            }
            catch (Exception ex)
            {
                rtbRespostas.AppendLine(ex.Message);
            }

        }

        private void btnConsultaDetalhe_Click(object sender, EventArgs e)
        {
            try
            {
                var ret = boleto.EnviarBoleto(OperacaoBoleto.tpConsultaDetalhe);
                rtbRespostas.AppendLine(ret.Retorno);
            }
            catch (Exception ex)
            {
                rtbRespostas.AppendLine(ex.Message);
            }

        }
    }
}