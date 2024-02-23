{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2022 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Italo Giurizzato Junior                         }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr    }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }
{                                                                              }
{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) }
{ qualquer versão posterior.                                                   }
{                                                                              }
{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM   }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU      }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)              }
{                                                                              }
{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto}
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,  }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Você também pode obter uma copia da licença em:                              }
{ http://www.opensource.org/licenses/lgpl-license.php                          }
{                                                                              }
{ Daniel Simões de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br}
{       Rua Coronel Aureliano de Camargo, 963 - Tatuí - SP - 18270-170         }
{******************************************************************************}

unit Frm_ACBrPagFor_Exemplo;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ComCtrls, IniFiles, ExtCtrls, ACBrPagFor, ACBrBase;

type
  TfrmACBrPagFor_Exemplo = class(TForm)
    btnGerar: TButton;
    Memo1: TMemo;
    Panel1: TPanel;
    lblColaborador: TLabel;
    lblPatrocinador: TLabel;
    lblDoar1: TLabel;
    lblDoar2: TLabel;
    gbConfiguracoes: TGroupBox;
    PageControl1: TPageControl;
    TabSheet4: TTabSheet;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    edtEmitCNPJ: TEdit;
    edtEmitIE: TEdit;
    edtEmitRazao: TEdit;
    edtEmitFantasia: TEdit;
    edtEmitFone: TEdit;
    edtEmitCEP: TEdit;
    edtEmitLogradouro: TEdit;
    edtEmitNumero: TEdit;
    edtEmitComp: TEdit;
    edtEmitBairro: TEdit;
    edtEmitCodCidade: TEdit;
    edtEmitCidade: TEdit;
    edtEmitUF: TEdit;
    TabSheet2: TTabSheet;
    btnSalvarConfig: TBitBtn;
    Label30: TLabel;
    cbBanco: TComboBox;
    ckSalvar: TCheckBox;
    edtPathSalvar: TEdit;
    sbtnPathSalvar: TSpeedButton;
    LogMsg: TMemo;
    Label1: TLabel;
    btnLinhaDig: TButton;
    btnCodBarras: TButton;
    btnMontarCodBarras: TButton;
    btnConvenio: TButton;
    ACBrPagFor1: TACBrPagFor;
    btnLer: TButton;
    OpenDialog1: TOpenDialog;
    Label2: TLabel;
    edtConvenio: TEdit;
    grpConta: TGroupBox;
    Label3: TLabel;
    edtAgencia: TEdit;
    Label4: TLabel;
    edtAgenciaDV: TEdit;
    Label5: TLabel;
    edtContaNumero: TEdit;
    Label6: TLabel;
    edtContaDV: TEdit;
    Label7: TLabel;
    edtDV: TEdit;
    Label8: TLabel;
    edtTipoConta: TEdit;
    chkUsarDadosConfig: TCheckBox;

    procedure sbtnPathSalvarClick(Sender: TObject);
    procedure btnSalvarConfigClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnGerarClick(Sender: TObject);
    procedure btnLinhaDigClick(Sender: TObject);
    {Pegar o vencimento da linha digitavel}
    function ExtrairValor(const CodigoBarras: String): Currency;
    function ExtrairDataVencimento(const CodigoBarras: String): TDateTime;
    procedure btnCodBarrasClick(Sender: TObject);
    procedure btnMontarCodBarrasClick(Sender: TObject);
    procedure btnConvenioClick(Sender: TObject);
    procedure lblColaboradorClick(Sender: TObject);
    procedure lblPatrocinadorClick(Sender: TObject);
    procedure lblDoar1Click(Sender: TObject);
    procedure lblDoar2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnLerClick(Sender: TObject);
  private
    { Private declarations }
    procedure GravarConfiguracao;
    procedure LerConfiguracao;
    procedure ConfiguraComponente;
    procedure AlimentarComponente;
  public
    { Public declarations }
  end;

var
  frmACBrPagFor_Exemplo: TfrmACBrPagFor_Exemplo;

implementation

uses
 FileCtrl, DateUtils, TypInfo,
 ACBrUtil.FilesIO, ACBrUtil.Strings,
 ACBrPagForConversao;

const
  SELDIRHELP = 1000;

{$R *.dfm}

procedure TfrmACBrPagFor_Exemplo.GravarConfiguracao;
var
  IniFile: String;
  Ini: TIniFile;
begin
  IniFile := ChangeFileExt( Application.ExeName, '.ini');

  Ini := TIniFile.Create( IniFile );

  try
    Ini.WriteString( 'Emitente', 'CNPJ'       , edtEmitCNPJ.Text);
    Ini.WriteString( 'Emitente', 'IE'         , edtEmitIE.Text);
    Ini.WriteString( 'Emitente', 'RazaoSocial', edtEmitRazao.Text);
    Ini.WriteString( 'Emitente', 'Fantasia'   , edtEmitFantasia.Text);
    Ini.WriteString( 'Emitente', 'Fone'       , edtEmitFone.Text);
    Ini.WriteString( 'Emitente', 'CEP'        , edtEmitCEP.Text);
    Ini.WriteString( 'Emitente', 'Logradouro' , edtEmitLogradouro.Text);
    Ini.WriteString( 'Emitente', 'Numero'     , edtEmitNumero.Text);
    Ini.WriteString( 'Emitente', 'Complemento', edtEmitComp.Text);
    Ini.WriteString( 'Emitente', 'Bairro'     , edtEmitBairro.Text);
    Ini.WriteString( 'Emitente', 'CodCidade'  , edtEmitCodCidade.Text);
    Ini.WriteString( 'Emitente', 'Cidade'     , edtEmitCidade.Text);
    Ini.WriteString( 'Emitente', 'UF'         , edtEmitUF.Text);

    Ini.WriteInteger( 'Banco', 'Banco'          , cbBanco.ItemIndex);
    Ini.WriteBool(    'Banco', 'Salvar'         , ckSalvar.Checked);
    Ini.WriteString(  'Banco', 'PathSalvar'     , edtPathSalvar.Text);
    Ini.WriteBool(    'Banco', 'UsarDadosConfig', chkUsarDadosConfig.Checked);

    Ini.WriteString( 'Conta', 'Convenio'   , edtConvenio.Text);
    Ini.WriteString( 'Conta', 'Agencia'    , edtAgencia.Text);
    Ini.WriteString( 'Conta', 'AgenciaDV'  , edtAgenciaDV.Text);
    Ini.WriteString( 'Conta', 'ContaNumero', edtContaNumero.Text);
    Ini.WriteString( 'Conta', 'ContaDV'    , edtContaDV.Text);
    Ini.WriteString( 'Conta', 'DV'         , edtDV.Text);
    Ini.WriteString( 'Conta', 'TipoConta'  , edtTipoConta.Text);
 finally
    Ini.Free;
  end;
end;

procedure TfrmACBrPagFor_Exemplo.lblColaboradorClick(Sender: TObject);
begin
  OpenURL('http://acbr.sourceforge.net/drupal/?q=node/5');
end;

procedure TfrmACBrPagFor_Exemplo.lblDoar1Click(Sender: TObject);
begin
  OpenURL('http://acbr.sourceforge.net/drupal/?q=node/14');
end;

procedure TfrmACBrPagFor_Exemplo.lblDoar2Click(Sender: TObject);
begin
  OpenURL('http://acbr.sourceforge.net/drupal/?q=node/14');
end;

procedure TfrmACBrPagFor_Exemplo.lblPatrocinadorClick(Sender: TObject);
begin
  OpenURL('http://acbr.sourceforge.net/drupal/?q=node/5');
end;

procedure TfrmACBrPagFor_Exemplo.LerConfiguracao;
var
  IniFile: String;
  Ini: TIniFile;
begin
  IniFile := ChangeFileExt( Application.ExeName, '.ini');

  Ini := TIniFile.Create( IniFile );

  try
    edtEmitCNPJ.Text       := Ini.ReadString( 'Emitente', 'CNPJ'       , '');
    edtEmitIE.Text         := Ini.ReadString( 'Emitente', 'IE'         , '');
    edtEmitRazao.Text      := Ini.ReadString( 'Emitente', 'RazaoSocial', '');
    edtEmitFantasia.Text   := Ini.ReadString( 'Emitente', 'Fantasia'   , '');
    edtEmitFone.Text       := Ini.ReadString( 'Emitente', 'Fone'       , '');
    edtEmitCEP.Text        := Ini.ReadString( 'Emitente', 'CEP'        , '');
    edtEmitLogradouro.Text := Ini.ReadString( 'Emitente', 'Logradouro' , '');
    edtEmitNumero.Text     := Ini.ReadString( 'Emitente', 'Numero'     , '');
    edtEmitComp.Text       := Ini.ReadString( 'Emitente', 'Complemento', '');
    edtEmitBairro.Text     := Ini.ReadString( 'Emitente', 'Bairro'     , '');
    edtEmitCodCidade.Text  := Ini.ReadString( 'Emitente', 'CodCidade'  , '');
    edtEmitCidade.Text     := Ini.ReadString( 'Emitente', 'Cidade'     , '');
    edtEmitUF.Text         := Ini.ReadString( 'Emitente', 'UF'         , '');

    cbBanco.ItemIndex  := Ini.ReadInteger('Banco', 'Banco'     , 0);
    ckSalvar.Checked   := Ini.ReadBool(   'Banco', 'Salvar'    , True);
    edtPathSalvar.Text := Ini.ReadString( 'Banco', 'PathSalvar', '');

    chkUsarDadosConfig.Checked := Ini.ReadBool('Banco', 'UsarDadosConfig', False);

    edtConvenio.Text    := Ini.ReadString( 'Conta', 'Convenio'   , '');
    edtAgencia.Text     := Ini.ReadString( 'Conta', 'Agencia'    , '');
    edtAgenciaDV.Text   := Ini.ReadString( 'Conta', 'AgenciaDV'  , '');
    edtContaNumero.Text := Ini.ReadString( 'Conta', 'ContaNumero', '');
    edtContaDV.Text     := Ini.ReadString( 'Conta', 'ContaDV'    , '');
    edtDV.Text          := Ini.ReadString( 'Conta', 'DV'         , '');
    edtTipoConta.Text   := Ini.ReadString( 'Conta', 'TipoConta'  , '');
  finally
    Ini.Free;
  end;
end;

procedure TfrmACBrPagFor_Exemplo.ConfiguraComponente;
begin
  ACBrPagFor1.Configuracoes.Arquivos.Salvar           := ckSalvar.Checked;
  ACBrPagFor1.Configuracoes.Arquivos.SepararPorCNPJ   := True;
  ACBrPagFor1.Configuracoes.Arquivos.SepararPorMes    := True;
  ACBrPagFor1.Configuracoes.Arquivos.AdicionarLiteral := True;
  ACBrPagFor1.Configuracoes.Arquivos.PathSalvar       := edtPathSalvar.Text;

  with ACBrPagFor1.Configuracoes.Geral do
  begin
    with Empresa do
    begin
      TipoInscricao := tiCNPJ;
      NumeroInscricao := edtEmitCNPJ.Text;
      Convenio := edtConvenio.Text;
      Nome := edtEmitRazao.Text;

      Conta.AgenciaCodigo := strToIntDef(edtAgencia.Text, 0);
      Conta.AgenciaDV := edtAgenciaDV.Text;
      Conta.TipoConta := strToIntDef(edtTipoConta.Text, 0);
      Conta.ContaNumero := strToInt64Def(edtContaNumero.Text, 0);
      Conta.ContaDV := edtContaDV.Text;
      Conta.DV := edtDV.Text;

      Endereco.Logradouro := edtEmitLogradouro.Text;
      Endereco.Numero := edtEmitNumero.Text;
      Endereco.Complemento := edtEmitComp.Text;
      Endereco.Cidade := edtEmitCidade.Text;
      Endereco.CEP := StrToIntDef(edtEmitCEP.Text, 0);
      Endereco.Estado := edtEmitUF.Text;
    end;

    UsarDadosConfig := chkUsarDadosConfig.Checked;
  end;

  ACBrPagFor1.Configuracoes.Geral.SubstitutaBanco := pagNenhum;

  // A propriedade Banco deve ser a ultima a ser definida
  ACBrPagFor1.Configuracoes.Geral.Banco := TBanco(cbBanco.ItemIndex);
end;

procedure TfrmACBrPagFor_Exemplo.btnConvenioClick(Sender: TObject);
var
  Linha, aux: String;
begin
  linha := '836500000010353501620002001010201620165942189177';

  LogMsg.Lines.add('------------------------------------------------------------------------');

  aux := copy(linha, 1,11) + '.' + copy(linha,13,11) + '.' +
         copy(linha,25,11) + '.' + copy(linha,33, 1) + '.' + copy(linha,38,10);

  LogMsg.Lines.add(aux);
end;

function TfrmACBrPagFor_Exemplo.ExtrairDataVencimento(const CodigoBarras: String): TDateTime;
begin
  if length(CodigoBarras) = 47 then
    Result := StrToDate('07/10/1997') + StrToInt(Copy(CodigoBarras, 34, 4)) // Pela Linha digitavel 47
  else
    Result := StrToDate('07/10/1997') + StrToInt(Copy(CodigoBarras, 6, 4)); // Pelo CodBarras       44
end;

function TfrmACBrPagFor_Exemplo.ExtrairValor(const CodigoBarras: String): Currency;
begin
  if length(CodigoBarras) = 47 then
    Result := StrToCurr(Copy(CodigoBarras, 38, 10)) / 100  // Pela Linha digitavel
  else
    Result := StrToCurr(Copy(CodigoBarras, 10, 10)) / 100; // Pelo COdBarras
end;

procedure TfrmACBrPagFor_Exemplo.sbtnPathSalvarClick(Sender: TObject);
var
  Dir : string;
begin
  if Length(edtPathSalvar.Text) <= 0 then
    Dir := ExtractFileDir(application.ExeName)
  else
    Dir := edtPathSalvar.Text;

  if SelectDirectory(Dir, [sdAllowCreate, sdPerformCreate, sdPrompt],SELDIRHELP) then
    edtPathSalvar.Text := Dir;
end;

procedure TfrmACBrPagFor_Exemplo.btnSalvarConfigClick(Sender: TObject);
begin
  GravarConfiguracao;
  ConfiguraComponente;
  LogMsg.Lines.Add('Novos de dados de Configuração Salvos.');
  LogMsg.Lines.Add('Componente Configurado com os novos dados.');
  LogMsg.Lines.Add(' ');
end;

procedure TfrmACBrPagFor_Exemplo.btnLinhaDigClick(Sender: TObject);
begin
  ShowMessage(DateToStr(ExtrairDataVencimento('03399704101600003328820476001027168420000000600'))); //linha dg
  ShowMessage(FloatToStr(ExtrairValor('03399704101600003328820476001027168420000000600')));
end;

procedure TfrmACBrPagFor_Exemplo.btnCodBarrasClick(Sender: TObject);
begin
  ShowMessage(DateToStr(ExtrairDataVencimento('03391684200000006009704116000033282047600102')));   //cod barra.
  ShowMessage(FloatToStr(ExtrairValor('03391684200000006009704116000033282047600102')));
end;

procedure TfrmACBrPagFor_Exemplo.btnMontarCodBarrasClick(Sender: TObject);
var
  Linha, banco, moeda, aux,
  p5barras, p6a9barras, p10a19barras,
  p20a24barras, p25a34barras, p356a44barras: String;
begin
  linha := '03399704101600003328820476001027168420000000600';

  aux := copy(linha, 01, 01) + copy(linha, 02, 03) + copy(linha, 33, 01) +
         copy(linha, 34, 04) + copy(linha, 38, 10) + copy(linha, 05, 05) +
         copy(linha, 11, 10) + copy(linha, 22, 10);

  LogMsg.Lines.add(aux);

  LogMsg.Lines.add('------------------------------------------------------------------------');

  banco := copy(linha,2,3);
  moeda := copy(linha,4,1);

  p20a24barras  := copy(linha, 05, 05);
  p25a34barras  := copy(linha, 11, 10);
  p356a44barras := copy(linha, 22, 10);

  p5barras     := copy(linha, 33, 01);
  p6a9barras   := copy(linha, 34, 04);
  p10a19barras := copy(linha, 38, 10);

  aux := copy(linha, 01, 01) + banco + p5barras + p6a9barras + p10a19barras +
         p20a24barras + p25a34barras + p356a44barras;

  LogMsg.Lines.add(aux);
end;

procedure TfrmACBrPagFor_Exemplo.FormCreate(Sender: TObject);
var
  A: TBanco;
begin
  cbBanco.Items.Clear;
  for A := Low(TBanco) to High(TBanco) do
    cbBanco.Items.Add(GetEnumName(TypeInfo(TBanco), integer(A)));
  cbBanco.ItemIndex := 0;
end;

procedure TfrmACBrPagFor_Exemplo.FormShow(Sender: TObject);
begin
  LerConfiguracao;
  ConfiguraComponente;
  LogMsg.Clear;
end;

procedure TfrmACBrPagFor_Exemplo.AlimentarComponente;
var
  ok: boolean;
  TotalLote: Double;
begin
  ACBrPagFor1.Arquivos.Clear;

  with ACBrPagFor1 do
  begin
    with Arquivos.New.PagFor do
    begin
      //////////////////////////////////////////////////////////////////////////
      // Geral
      //////////////////////////////////////////////////////////////////////////
      {
       TTipoServico     = (tsCobranca, tsBloquetoEletronico, tsConciliacaoBancaria,
                           tsDebitos, tsCustodiaCheques, tsGestaoCaixa,
                           tsConsultaMargem, tsAverbacaoConsignacao,
                           tsPagamentoDividendos, tsManutencaoConsignacao,
                           tsConsignacaoParcelas, tsGlosaConsignacao,
                           tsConsultaTributosaPagar, tsPagamentoFornecedor,
                           tsPagamentoContas, tsCompror, tsComprorRotativo,
                           tsAlegacaoSacado, tsPagamentoSalarios,
                           tsPagamentoHonorarios, tsPagamentoBolsaAuxilio,
                           tsPagamentoPrebenda, tsVendor, tsVendoraTermo,
                           tsPagamentoSinistrosSegurado, tsPagamentoDespesaViagem,
                           tsPagamentoAutorizado, tsPagamentoCredenciados,
                           tsPagamentoRemuneracao, tsPagamentoRepresentantes,
                           tsPagamentoBeneficios, tsPagamentosDiversos);
      }

      // Usado pela caixa (T=Teste; P=Produção)
      Geral.AmbienteCliente := 'T';

      Geral.Banco := Configuracoes.Geral.Banco;
      // Utilizado Somente quando houver o segmento C
      Geral.SubstitutaBanco := Configuracoes.Geral.SubstitutaBanco;

      //////////////////////////////////////////////////////////////////////////
      // Registro 0
      //////////////////////////////////////////////////////////////////////////

      // As linhas que alimentam os campos da classe Empresa podem ser omitidas
      // caso o componente seja configurado com essas informações e a propriedade
      // UsarDadosConfig estiver com o valor True
      // Observação: Somente Registro0 e Registro1
      {
      // TTipoInscricao   = (tiIsento, tiCPF, tiCNPJ, tiPISPASEP, tiOutros);
      Registro0.Empresa.Inscricao.Tipo := tiCNPJ;
      Registro0.Empresa.Inscricao.Numero := edtEmitCNPJ.Text;
      Registro0.Empresa.Convenio := '930172';
      Registro0.Empresa.ContaCorrente.Agencia.Codigo := 151;
      Registro0.Empresa.ContaCorrente.Agencia.DV := '';
      Registro0.Empresa.ContaCorrente.Conta.Numero := 232270;
      Registro0.Empresa.ContaCorrente.Conta.DV := '0';
      Registro0.Empresa.ContaCorrente.DV := ' ';
      Registro0.Empresa.ContaCorrente.Conta.TipoConta := 1;
      Registro0.Empresa.Nome := edtEmitRazao.Text;
      }
      Registro0.NomeBanco := BancoToDesc(Geral.Banco);
      Registro0.Arquivo.Codigo := taRemessa;
      Registro0.Arquivo.DataGeracao := Date;
      Registro0.Arquivo.HoraGeracao := Time;
      Registro0.Arquivo.Sequencia := 1;
      Registro0.Arquivo.Densidade := 01600;

      // Usado pela Caixa (Parâmetro de Transmissão) Código Informado pelo Banco
      Registro0.Arquivo.ParamTransm := '00';

      Registro0.ReservadoBanco := '';
      Registro0.ReservadoEmpresa := '';

      // Usado pelo Banco do Brasil (True = Teste, False = Produção)
      Registro0.RemessaTeste := True;

      with Lote.New do
      begin
        ////////////////////////////////////////////////////////////////////////
        // Antes de Executar o Comando GerarLote deve-se informar os dados dos
        // Registro 1,
        // Segmentos (SegmentoA, SegmentoB, ... dependendo do que vai ser pago)
        //           (consultar o manual da FEBRABAN 240 Posições             )
        // e Registro 5
        ////////////////////////////////////////////////////////////////////////

        ////////////////////////////////////////////////////////////////////////
        // Registro 1
        ////////////////////////////////////////////////////////////////////////
        TotalLote := 0.0;

        Registro1.Servico.Operacao := toCredito;
        Registro1.Servico.TipoServico := tsPagamentoFornecedor;
        Registro1.Servico.FormaLancamento := flDocTed;

        // As linhas que alimentam os campos da classe Empresa podem ser omitidas
        // caso o componente seja configurado com essas informações e a propriedade
        // UsarDadosConfig estiver com o valor True
        // Observação: Somente Registro0 e Registro1
        {
        Registro1.Empresa.Inscricao.Tipo := tiCNPJ;
        Registro1.Empresa.Inscricao.Numero := edtEmitCNPJ.Text;
        Registro1.Empresa.Convenio := '930172';
        Registro1.Empresa.ContaCorrente.Agencia.Codigo := 151;
        Registro1.Empresa.ContaCorrente.Agencia.DV := '';
        Registro1.Empresa.ContaCorrente.Conta.Numero := 232270;
        Registro1.Empresa.ContaCorrente.Conta.DV := '0';
        Registro1.Empresa.ContaCorrente.DV := ' ';
        Registro1.Empresa.ContaCorrente.Conta.TipoConta := 1;
        Registro1.Empresa.Nome := edtEmitRazao.Text;
        }
        Registro1.Informacao1 := '';

        // As linhas que alimentam os campos da classe Endereco podem ser omitidas
        // caso o componente seja configurado com essas informações e a propriedade
        // UsarDadosConfig estiver com o valor True
        // Observação: Somente Registro1
        {
        Registro1.Endereco.Logradouro := edtEmitLogradouro.Text;
        Registro1.Endereco.Numero := edtEmitNumero.Text;
        Registro1.Endereco.Complemento := edtEmitComp.Text;
        Registro1.Endereco.Cidade := edtEmitCidade.Text;
        Registro1.Endereco.CEP := StrToIntDef(edtEmitCEP.Text, 0);
        Registro1.Endereco.Estado := edtEmitUF.Text;
        }
        Registro1.IndFormaPag := ifpDebitoContaCorrente;

        // Usado pela Caixa
        Registro1.TipoCompromisso := 1;
        Registro1.CodigoCompromisso := 1;

        ////////////////////////////////////////////////////////////////////////
        // Segmento A - Exemplo de pagamento via DOC
        ////////////////////////////////////////////////////////////////////////
        with SegmentoA.New do
        begin
          CodMovimento := imInclusaoRegistroDetalheLiberado;
          TipoMovimento:= tmInclusao;

          with Favorecido do
          begin
            // “018” para “TED” (Valor igual ou superior a R$ 0,01);
            // “700” para “DOC Eletrônico” (Valor inferior a R$ 5.000,00); ou
            // “888” para “TED” (Qualquer valor) quando for necessário o envio
            // de TED utilizando o código ISPB.
            Camara := 700;
            Banco := pagItau;
            ContaCorrente.Agencia.Codigo  := 0643;
            ContaCorrente.Agencia.DV      := '';
            ContaCorrente.Conta.TipoConta := 1;
            ContaCorrente.Conta.Numero    := 04325;
            ContaCorrente.Conta.DV        := '0';
            Nome                          := 'DAVI DE SOUZA DA SILVA';
          end;

          with Credito do
          begin
            SeuNumero      := '159357';
            DataPagamento  := StrToDate('25/07/2016');
            Moeda.Tipo     := tmReal;
            Moeda.Qtde     := 0.0;
            ValorPagamento := 100.0;
            NossoNumero    := '';
            DataReal       := StrToDate('25/07/2016');
            ValorReal      := 100.0;

            // Usado pela Caixa
            QtdeParcelas   := 1;
            IndBloqueio    := 'N';
            FormaParcelamento := 1;
            DiaVencimento  := 5;
            NumParcela := 0;
          end;

          Informacao2 := '';
          CodigoDOC   := '';
          CodigoTED   := '';
          CodigoComp  := '';
          Aviso       := 0;

          TotalLote := TotalLote + Credito.ValorPagamento;

          with SegmentoB.New do
          begin
            with Inscricao do
            begin
              Tipo   := tiCPF;
              Numero := '08201178971';
            end;

            with Endereco do
            begin
              Logradouro  := 'ROD. JOÃO ALFREDO ROSA';
              Numero      := '1';
              Complemento := '';
              Bairro      := 'GUARDA MARGEM ESQUERDA';
              Cidade      := 'TUBARÃO';
              CEP         := 88702704;
              Estado      := 'SC';
            end;

            DataVencimento:= StrToDate('25/07/2016');
            Valor         := 100.00;
            Abatimento    := 0.00;
            Desconto      := 0.00;
            Mora          := 0.00;
            Multa         := 0.00;
            CodigoDoc     := '';
            Aviso         := 0;
            CodigoUG      := 0;
          end;
        end;

        ////////////////////////////////////////////////////////////////////////
        // Segmento A - Exemplo de pagamento via TED
        ////////////////////////////////////////////////////////////////////////
        with SegmentoA.New do
        begin
          CodMovimento  := imInclusaoRegistroDetalheLiberado;
          TipoMovimento := tmInclusao;

          with Favorecido do
          begin
            // “018” para “TED” (Valor igual ou superior a R$ 0,01);
            // “700” para “DOC Eletrônico” (Valor inferior a R$ 5.000,00); ou
            // “888” para “TED” (Qualquer valor) quando for necessário o envio
            // de TED utilizando o código ISPB.
            Camara := 018;
            Banco := pagBancodoBrasil;
            ContaCorrente.Agencia.Codigo  := 0201;
            ContaCorrente.Agencia.DV      := '1';
            ContaCorrente.Conta.TipoConta := 1;
            ContaCorrente.Conta.Numero    := 49481;
            ContaCorrente.Conta.DV        := '0';
            Nome                          := 'DAVI DE SOUZA DA SILVA';
          end;

          with Credito do
          begin
            SeuNumero      := '123369';
            DataPagamento  := StrToDate('25/07/2016');
            Moeda.Tipo     := tmReal;
            Moeda.Qtde     := 0.0;
            ValorPagamento := 3000.0;
            NossoNumero    := '';
            DataReal       := StrToDate('25/07/2016');
            ValorReal      := 3000.0;
          end;

          Informacao2 := '';
          CodigoDOC   := '';
          CodigoTED   := '';
          //“CC” quando for Conta Corrente ou “PP” quando for Conta Poupança
          CodigoComp  := '';
          Aviso       := 0;

          TotalLote := TotalLote + Credito.ValorPagamento;

        ////////////////////////////////////////////////////////////////////////
        // Segmento B - Exemplo de pagamento via TED
        ////////////////////////////////////////////////////////////////////////
          with SegmentoB.New do
          begin
            with Inscricao do
            begin
              Tipo   := tiCPF;
              Numero := '08201178971';
            end;

            with Endereco do
            begin
              Logradouro  := 'ROD. JOÃO ALFREDO ROSA';
              Numero      := '1';
              Complemento := '';
              Bairro      := 'GUARDA MARGEM ESQUERDA';
              Cidade      := 'TUBARÃO';
              CEP         := 88702704;
              Estado      := 'SC';
            end;

            DataVencimento:= StrToDate('25/07/2016');
            Valor         := 3000.00;
            Abatimento    := 0.00;
            Desconto      := 0.00;
            Mora          := 0.00;
            Multa         := 0.00;
            CodigoDoc     := '';
            Aviso         := 0;
            CodigoUG      := 0;
          end;
        end;

        ////////////////////////////////////////////////////////////////////////
        // Registro 5
        ////////////////////////////////////////////////////////////////////////
        Registro5.Valor := TotalLote;
        Registro5.QtdeMoeda := 0.0;
        Registro5.NumAvisoDebito := 0;
      end;

      with Lote.New do
      begin
        ////////////////////////////////////////////////////////////////////////
        // Antes de Executar o Comando GerarLote deve-se informar os dados dos
        // Registro 1,
        // Segmentos (SegmentoA, SegmentoB, ... dependendo do que vai ser pago)
        //           (consultar o manual da FEBRABAN 240 Posições             )
        // e Registro 5
        ////////////////////////////////////////////////////////////////////////

        ////////////////////////////////////////////////////////////////////////
        // Registro 1
        ////////////////////////////////////////////////////////////////////////
        TotalLote := 0.0;

        Registro1.Servico.Operacao := toCredito;
        Registro1.Servico.TipoServico := tsPagamentoFornecedor;
        Registro1.Servico.FormaLancamento := flPIXTransferencia;
        Registro1.Empresa.Inscricao.Tipo := tiCNPJ;
        Registro1.Empresa.Inscricao.Numero := edtEmitCNPJ.Text;
        Registro1.Empresa.Convenio := '930172';
        Registro1.Empresa.ContaCorrente.Agencia.Codigo := 151;
        Registro1.Empresa.ContaCorrente.Agencia.DV := '';
        Registro1.Empresa.ContaCorrente.Conta.Numero := 232270;
        Registro1.Empresa.ContaCorrente.Conta.DV := '0';
        Registro1.Empresa.ContaCorrente.DV := ' ';
        Registro1.Empresa.ContaCorrente.Conta.TipoConta := 1;
        Registro1.Empresa.Nome := edtEmitRazao.Text;
        Registro1.Informacao1 := '';
        Registro1.Endereco.Logradouro := edtEmitLogradouro.Text;
        Registro1.Endereco.Numero := edtEmitNumero.Text;
        Registro1.Endereco.Complemento := edtEmitComp.Text;
        Registro1.Endereco.Cidade := edtEmitCidade.Text;
        Registro1.Endereco.CEP := StrToIntDef(edtEmitCEP.Text, 0);
        Registro1.Endereco.Estado := edtEmitUF.Text;

        ////////////////////////////////////////////////////////////////////////
        // Segmento A - Exemplo de pagamento via PIX
        ////////////////////////////////////////////////////////////////////////
        with SegmentoA.New do
        begin
          CodMovimento := imInclusaoRegistroDetalheLiberado;
          TipoMovimento:= tmInclusao;

          with Favorecido do
          begin
            // PIX
            Camara := 009;
//            Banco := pagItau;
//            ContaCorrente.Agencia.Codigo  := 0643;
//            ContaCorrente.Agencia.DV      := '';
//            ContaCorrente.Conta.TipoConta := 1;
//            ContaCorrente.Conta.Numero    := 04325;
//            ContaCorrente.Conta.DV        := '0';
//            Nome                          := 'DAVI DE SOUZA DA SILVA';
          end;

          with Credito do
          begin
            SeuNumero      := '159357';
            DataPagamento  := StrToDate('25/07/2016');
            Moeda.Tipo     := tmReal;
            Moeda.Qtde     := 0.0;
            ValorPagamento := 100.0;
            NossoNumero    := '';
            DataReal       := StrToDate('25/07/2016');
            ValorReal      := 100.0;
          end;

          {
            Formatação para identificação Pagamento via PIX
            CCCCCCCCCCCCCCIIIIIIIIRR, onde:
            C = Número de inscrição CNPJ (14 dígitos) ou Número
            de inscrição do CPF (11 digitos com 0 a esq)
            I = Código do ISPB (8 dígitos)
            R = Tipo de Conta  (2 dígitos = 01 – Conta corrente)
          }
          Informacao2 := '00008201178971' + BancoToIspb(Geral.Banco) + '01';
          CodigoDOC   := '';
          CodigoTED   := '';
          CodigoComp  := '';
          Aviso       := 0;

          TotalLote := TotalLote + Credito.ValorPagamento;

          ////////////////////////////////////////////////////////////////////////
          // Segmento B - Exemplo de pagamento via PIX
          ////////////////////////////////////////////////////////////////////////
          with SegmentoB.New do
          begin
            with Inscricao do
            begin
              Tipo   := tiCPF;
              Numero := '08201178971';
            end;

            CodigoISPB := StrToInt(BancoToIspb(Geral.Banco));
            PixTipoChave := tcpCPFCNPJ;
            PixMensagem := '';
            PixTXID := '';
            PixChave := '08201178971';

            {
            with Endereco do
            begin
              Logradouro  := 'ROD. JOÃO ALFREDO ROSA';
              Numero      := 1;
              Complemento := '';
              Bairro      := 'GUARDA MARGEM ESQUERDA';
              Cidade      := 'TUBARÃO';
              CEP         := 88702704;
              Estado      := 'SC';
            end;

            DataVencimento:= StrToDate('25/07/2016');
            Valor         := 100.00;
            Abatimento    := 0.00;
            Desconto      := 0.00;
            Mora          := 0.00;
            Multa         := 0.00;
            CodigoDoc     := '';
            Aviso         := 0;
            CodigoUG      := 0;
            }
          end;
        end;

        ////////////////////////////////////////////////////////////////////////
        // Registro 5
        ////////////////////////////////////////////////////////////////////////
        Registro5.Valor := TotalLote;
        Registro5.QtdeMoeda := 0.0;
        Registro5.NumAvisoDebito := 0;
      end;

      with Lote.New do
      begin
        ////////////////////////////////////////////////////////////////////////
        // Registro 1
        ////////////////////////////////////////////////////////////////////////
        TotalLote := 0.0;

        Registro1.Servico.Operacao := toCredito;
        Registro1.Servico.TipoServico := tsCobranca;
        Registro1.Servico.FormaLancamento := flLiquidacaoTitulosOutrosBancos;
        Registro1.Empresa.Inscricao.Tipo := tiCNPJ;
        Registro1.Empresa.Inscricao.Numero := edtEmitCNPJ.Text;
        Registro1.Empresa.Convenio := '930172';
        Registro1.Empresa.ContaCorrente.Agencia.Codigo := 151;
        Registro1.Empresa.ContaCorrente.Agencia.DV := '';
        Registro1.Empresa.ContaCorrente.Conta.Numero := 232270;
        Registro1.Empresa.ContaCorrente.Conta.DV := '0';
        Registro1.Empresa.ContaCorrente.DV := ' ';
        Registro1.Empresa.ContaCorrente.Conta.TipoConta := 1;
        Registro1.Empresa.Nome := edtEmitRazao.Text;
        Registro1.Informacao1 := '';
        Registro1.Endereco.Logradouro := edtEmitLogradouro.Text;
        Registro1.Endereco.Numero := edtEmitNumero.Text;
        Registro1.Endereco.Complemento := edtEmitComp.Text;
        Registro1.Endereco.Cidade := edtEmitCidade.Text;
        Registro1.Endereco.CEP := StrToIntDef(edtEmitCEP.Text, 0);
        Registro1.Endereco.Estado := edtEmitUF.Text;

        ////////////////////////////////////////////////////////////////////////
        // Segmento J
        ////////////////////////////////////////////////////////////////////////
        with SegmentoJ.New do
        begin
          TipoMovimento := tmInclusao;
          CodMovimento := imInclusaoRegistroDetalheLiberado;
          CodigoBarras := '23794686500000030007508091140600004204891150';
          NomeCedente := 'CASAS DAS BATERIAS PEÇAS E SERVIÇOS PARA AUTOMOVEIS LTDA EPP';
          DataVencimento := StrToDate('24/07/2016');
          ValorTitulo := 30.00;
          Desconto := 0.0;
          Acrescimo := 0.0;
          DataPagamento := StrToDate('24/07/2016');
          ValorPagamento := 30.00;
          QtdeMoeda := 0.0;
          ReferenciaSacado := '';
          CodigoMoeda := 09;

          // Usado pela Caixa
          SeuNumero := '123456';
          NossoNumero := '123456';

          with SegmentoJ52.New do
          begin
            TipoMovimento := tmInclusao;
            CodMovimento := imInclusaoRegistroDetalheLiberado;

            with Pagador do
            begin
              with Inscricao do
              begin
                Tipo := tiCNPJ;
                Numero := '12345678000123';
              end;

              Nome := 'Nome do Pagador';
            end;

            with Beneficiario do
            begin
              with Inscricao do
              begin
                Tipo := tiCNPJ;
                Numero := '12345678000123';
              end;

              Nome := 'Nome do Beneficiario';
            end;

            with SacadorAvalista do
            begin
              with Inscricao do
              begin
                Tipo := tiCNPJ;
                Numero := '12345678000123';
              end;

              Nome := 'Nome do Sacador/Avalista';
            end;

            Chave := '';
            TXID := '';
          end;

          TotalLote := TotalLote + ValorPagamento;
        end;

        ////////////////////////////////////////////////////////////////////////
        // Registro 5
        ////////////////////////////////////////////////////////////////////////
        Registro5.Valor := TotalLote;
        Registro5.QtdeMoeda := 0.0;
        Registro5.NumAvisoDebito := 0;
      end;

      with Lote.New do
      begin
        ////////////////////////////////////////////////////////////////////////
        // Registro 1
        ////////////////////////////////////////////////////////////////////////
        TotalLote := 0.0;

        Registro1.Servico.Operacao := toCredito;
        Registro1.Servico.TipoServico := tsPagamentoContas;
        Registro1.Servico.FormaLancamento := flPagamentoContas;
        Registro1.Empresa.Inscricao.Tipo := tiCNPJ;
        Registro1.Empresa.Inscricao.Numero := edtEmitCNPJ.Text;
        Registro1.Empresa.Convenio := '930172';
        Registro1.Empresa.ContaCorrente.Agencia.Codigo := 151;
        Registro1.Empresa.ContaCorrente.Agencia.DV := '';
        Registro1.Empresa.ContaCorrente.Conta.Numero := 232270;
        Registro1.Empresa.ContaCorrente.Conta.DV := '0';
        Registro1.Empresa.ContaCorrente.DV := ' ';
        Registro1.Empresa.ContaCorrente.Conta.TipoConta := 1;
        Registro1.Empresa.Nome := edtEmitRazao.Text;
        Registro1.Informacao1 := '';
        Registro1.Endereco.Logradouro := edtEmitLogradouro.Text;
        Registro1.Endereco.Numero := edtEmitNumero.Text;
        Registro1.Endereco.Complemento := edtEmitComp.Text;
        Registro1.Endereco.Cidade := edtEmitCidade.Text;
        Registro1.Endereco.CEP := StrToIntDef(edtEmitCEP.Text, 0);
        Registro1.Endereco.Estado := edtEmitUF.Text;

        ////////////////////////////////////////////////////////////////////////
        // Segmento O
        ////////////////////////////////////////////////////////////////////////
        with SegmentoO.New do
        begin
          TipoMovimento      := tmInclusao;
          CodMovimento       := imInclusaoRegistroDetalheLiberado;
          CodigoBarras       := '83650000001353501620000010102016216594218917';
          NomeConcessionaria := 'CELESC AD CEN';
          DataVencimento     := StrToDate('15/07/2016');
          DataPagamento      := StrToDate('15/07/2016');
          ValorPagamento     := 135.35;
          SeuNumero          := '080716';
          NossoNumero        := '';

          TotalLote := TotalLote + ValorPagamento;
        end;

        ////////////////////////////////////////////////////////////////////////
        // Registro 5
        ////////////////////////////////////////////////////////////////////////
        Registro5.Valor := TotalLote;
        Registro5.QtdeMoeda := 0.0;
        Registro5.NumAvisoDebito := 0;
      end;
    end;
  end;
end;

procedure TfrmACBrPagFor_Exemplo.btnGerarClick(Sender: TObject);
var
  NomeArquivo: String;
begin
  ForceDirectories(edtPathSalvar.Text);

  AlimentarComponente;

  NomeArquivo := edtPathSalvar.Text + '\Cpg'+FormatDateTime('DDMM', now) + '-' +
                 BancoToStr(ACBrPagFor1.Configuracoes.Geral.Banco) +
                 '.seq';

  ACBrPagFor1.GravarTxtRemessa(NomeArquivo);

  LogMsg.Lines.Add('Arquivo de Remessa: ' + NomeArquivo);
  LogMsg.Lines.Add('Gerado com sucesso.');
  LogMsg.Lines.Add(' ');
end;

procedure TfrmACBrPagFor_Exemplo.btnLerClick(Sender: TObject);
var
  NomeArquivo: String;
  i, k: Integer;
begin
  OpenDialog1.Title := 'Selecione o Arquivo de Retorno';
  OpenDialog1.DefaultExt := '*.txt';
  OpenDialog1.Filter :=
    'Arquivos TXT (*.txt)|*.txt|Todos os Arquivos (*.*)|*.*';
  OpenDialog1.InitialDir := ACBrPagFor1.Configuracoes.Arquivos.PathSalvar;

  if OpenDialog1.Execute then
  begin
    NomeArquivo := OpenDialog1.FileName;

    ACBrPagFor1.LerTxtRetorno(NomeArquivo);

    LogMsg.Lines.Add('Arquivo de Retorno: ' + NomeArquivo);
    LogMsg.Lines.Add('Lido com sucesso.');
    LogMsg.Lines.Add(' ');

    if ACBrPagFor1.Arquivos.Count > 0 then
      LogMsg.Lines.Add('Lista de Ocorrências:')
    else
      LogMsg.Lines.Add('Sem nenhuma Ocorrência.');

    for i := 0 to ACBrPagFor1.Arquivos.Count -1 do
    begin
      for k := 0 to ACBrPagFor1.Arquivos.Items[i].PagFor.Registro0.Aviso.Count -1 do
      begin
        with ACBrPagFor1.Arquivos.Items[i].PagFor.Registro0.Aviso.Items[k] do
        begin
          LogMsg.Lines.Add('Código........:' + CodigoRetorno);
          LogMsg.Lines.Add('Mensagem......:' + MensagemRetorno);
          LogMsg.Lines.Add('Segmento......:' + Segmento);
          LogMsg.Lines.Add('Segmento Filho:' + SegmentoFilho);
          LogMsg.Lines.Add('Seu Numero....:' + SeuNumero);
        end;
      end;
    end;
  end;
end;

end.
