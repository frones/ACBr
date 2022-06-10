{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
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

{$I ACBr.inc}

unit ACBrTEFDPayGo;

interface

uses
  Classes, SysUtils,
  ACBrTEFDClass, ACBrTEFComum;

const
  CACBrTEFDPayGoNome = 'ACBrTEFDPayGo';
  CACBrTEFDPayGoVersao = '1.0.2';
  CACBrTEFDPayGoVersaoInterface = '225';

  CACBrTEFDPayGo_ArqTemp = 'C:\PAYGO\REQ\intpos.tmp';
  CACBrTEFDPayGo_ArqReq  = 'C:\PAYGO\REQ\intpos.001';
  CACBrTEFDPayGo_ArqResp = 'C:\PAYGO\RESP\intpos.001';
  CACBrTEFDPayGo_ArqSTS  = 'C:\PAYGO\RESP\intpos.sts';
  CACBrTEFDDial_ArqTemp   = 'C:\TEF_DIAL\req\intpos.tmp';
  CACBrTEFDDial_ArqReq    = 'C:\TEF_DIAL\req\intpos.001';
  CACBrTEFDDial_ArqResp   = 'C:\TEF_DIAL\resp\intpos.001';
  CACBrTEFDDial_ArqSTS    = 'C:\TEF_DIAL\resp\intpos.sts';
  CACBrTEFDDial_GPExeName = 'C:\TEF_DIAL\tef_dial.exe';

type

  { TACBrTEFDRespPayGo }

  TACBrTEFDRespPayGo = class( TACBrTEFDRespTXT )
  public
    procedure ProcessarTipoInterno(ALinha: TACBrTEFLinha); override;
    procedure ConteudoToProperty; override;
  end;

  { TACBrTEFDPayGo }

  TACBrTEFDPayGo = class( TACBrTEFDClassTXT )
  private
    fSuportaReajusteValor : Boolean;
    fSuportaNSUEstendido: Boolean;
    fSuportaViasDiferenciadas: Boolean;

  protected
    procedure AdicionarIdentificacao; override;
    procedure ProcessarResposta; override;
    procedure FinalizarRequisicao; override;

  public
    constructor Create(AOwner: TComponent); override;
    procedure Inicializar; override;

  published
    Property SuportaReajusteValor: Boolean read fSuportaReajusteValor write fSuportaReajusteValor default False;
    Property SuportaNSUEstendido: Boolean read fSuportaNSUEstendido write fSuportaNSUEstendido default True;
    Property SuportaViasDiferenciadas: Boolean read fSuportaViasDiferenciadas write fSuportaViasDiferenciadas default True;
  end;

implementation

uses
  math, Types, strutils,
  ACBrTEFD, ACBrTEFPayGoRedes,
  ACBrUtil.Base,
  ACBrConsts;

{ TACBrTEFDRespPayGo }

procedure TACBrTEFDRespPayGo.ConteudoToProperty;
var
  IndiceViaCliente, Linhas, I: Integer;
  ViasDeComprovante, TipoDeCartao, TipoDeFinanciamento, StatusConfirmacao: String;
  ImprimirViaCliente, ImprimirViaEstabelecimento, CupomReduzido: Boolean;

  function LinhasDaViaDoCliente( ViaReduzida: Boolean ): Integer;
  begin
    if ViaReduzida then
      Result := LeInformacao(710, 0).AsInteger     // 710-000 Linhas Via Reduzida Cliente
    else
      Result := LeInformacao(712, 0).AsInteger;    // 712-000 Tamanho Via Normal Cliente
  end;

begin
  inherited ConteudoToProperty;

  // 737-000 Vias de Comprovante
  //   1: imprimir somente a via do Cliente
  //   2: imprimir somente a via do Estabelecimento
  //   3: imprimir ambas as vias do Cliente e do Estabelecimento
  ViasDeComprovante := Trim(LeInformacao(737, 0).AsString);
  if (ViasDeComprovante = '') then
    ViasDeComprovante := '3';

  ImprimirViaCliente := (ViasDeComprovante = '1') or (ViasDeComprovante = '3');
  ImprimirViaEstabelecimento := (ViasDeComprovante = '2') or (ViasDeComprovante = '3');

  // Verificando Via do Estabelecimento
  if ImprimirViaEstabelecimento then
  begin
    Linhas := LeInformacao(714, 0).AsInteger;    // 714-000 Linhas Via Estabelecimento;
    if (Linhas > 0) then
    begin
      fpImagemComprovante2aVia.Clear;
      For I := 1 to Linhas do
        fpImagemComprovante2aVia.Add( AjustaLinhaImagemComprovante(LeInformacao(715, I).AsString) );
    end
    else
    begin
      // Não informado específico, usa Via Unica (029-000), lida em TACBrTEFDRespTXT.ConteudoToProperty
      fpImagemComprovante2aVia.Text := fpImagemComprovante1aVia.Text;
    end;
  end
  else
    fpImagemComprovante2aVia.Clear;

  // Verificando Via do Cliente
  if ImprimirViaCliente then
  begin
    IndiceViaCliente := 0;
    CupomReduzido := ViaClienteReduzida;
    Linhas := LinhasDaViaDoCliente( CupomReduzido );
    if (Linhas = 0) then  // Se não achou, veja se tem algo no outro tipo
    begin
      CupomReduzido := not CupomReduzido;
      Linhas := LinhasDaViaDoCliente( CupomReduzido );
    end;

    // Se não foi informado específico, usará Via Unica (029-000), já carregada em TACBrTEFDRespTXT.ConteudoToProperty
    if (Linhas > 0) then
    begin
      if CupomReduzido then
        IndiceViaCliente := 711
      else
        IndiceViaCliente := 713;

      fpImagemComprovante1aVia.Clear;
      For I := 1 to Linhas do
        fpImagemComprovante1aVia.Add( AjustaLinhaImagemComprovante(LeInformacao(IndiceViaCliente, I).AsString) );
    end;
  end
  else
    fpImagemComprovante1aVia.Clear;

  fpQtdLinhasComprovante := max(fpImagemComprovante1aVia.Count, fpImagemComprovante2aVia.Count);

  // 731-000 - Tipo de cartão
  //     0: qualquer / não definido (padrão)
  //     1: crédito
  //     2: débito
  //     3: voucher
  TipoDeCartao := Trim(LeInformacao(731, 0).AsString);
  if (TipoDeCartao <> '') then
  begin
    fpCredito := (TipoDeCartao = '1');
    fpDebito  := (TipoDeCartao = '2');
  end;

  // 732-000 - Tipo de financiamento
  //     0: qualquer / não definido (padrão)
  //     1: à vista
  //     2: parcelado pelo Emissor
  //     3: parcelado pelo Estabelecimento
  //     4: pré-datado
  //     5: pré-datado forçado
  TipoDeFinanciamento := Trim(LeInformacao(732, 0).AsString);
  if (TipoDeFinanciamento <> '') then
  begin
    if (TipoDeFinanciamento = '1') then
    begin
      fpTipoOperacao := opAvista;
      fpParceladoPor := parcNenhum;
    end

    else if (TipoDeFinanciamento = '2') then
    begin
      fpTipoOperacao := opParcelado;
      fpParceladoPor := parcADM;
    end

    else if (TipoDeFinanciamento = '2') then
    begin
      fpTipoOperacao := opParcelado;
      fpParceladoPor := parcADM;
    end

    else if (TipoDeFinanciamento = '3') then
    begin
      fpTipoOperacao := opParcelado;
      fpParceladoPor := parcLoja;
    end

    else if (TipoDeFinanciamento = '4') or (TipoDeFinanciamento = '5') then
      fpTipoOperacao := opPreDatado;
  end;

  fpNFCeSAT.Autorizacao := fpNSU;

  // 729-000 - Status da confirmação
  //    1: transação não requer confirmação, ou já confirmada
  //    2: transação requer confirmação
  //    Se não encontrado, assumir que a transação requer confirmação se houver comprovantes a serem impressos.
  StatusConfirmacao := Trim(LeInformacao(729,0).AsString);
  if (StatusConfirmacao <> '') then
    fpConfirmar := (StatusConfirmacao = '2');
end;

procedure TACBrTEFDRespPayGo.ProcessarTipoInterno(ALinha: TACBrTEFLinha);
var
  ARede: TACBrTEFPayGoRede;
  CodRede: Integer;
begin
  case ALinha.Identificacao of
    707 : fpValorOriginal := ALinha.Informacao.AsFloat;
    708 : fpSaque         := ALinha.Informacao.AsFloat;
    709 : fpDesconto      := ALinha.Informacao.AsFloat;
    727 : fpTaxaServico   := ALinha.Informacao.AsFloat;
    739 :
    begin
      // 739-000 Índice da Rede Adquirente
      fpCodigoRedeAutorizada := ALinha.Informacao.AsString;
      fpNFCeSAT.CodCredenciadora := fpCodigoRedeAutorizada;
      CodRede := StrToIntDef(fpCodigoRedeAutorizada, 0);
      if (CodRede > 0) then
      begin
        ARede := TabelaRedes.Find(CodRede);
        if Assigned(ARede) then
        begin
          fpNFCeSAT.Bandeira := ARede.NomeTrad;
          fpNFCeSAT.CNPJCredenciadora := ARede.CNPJ;
          fpNFCeSAT.CodCredenciadora := IntToStrZero(ARede.CodSATCFe, 3);
        end;
      end;
    end;
    740 :
    begin
      // 740-000 - Número do cartão, mascarado
      fpBin := ALinha.Informacao.AsString;
      fpNFCeSAT.UltimosQuatroDigitos := RightStr(fpBin,4);
    end;
    741 :
    begin
      // 741-000 - Nome do Cliente, extraído do cartão ou informado pelo emissor.
      fpNFCeSAT.DonoCartao := ALinha.Informacao.AsString;
    end;
    747 :
    begin
      // 747-000 - Data de vencimento do cartão (MMAA)
      fpNFCeSAT.DataExpiracao := ALinha.Informacao.AsString;
    end;
    748 :
    begin
      // 748-000 - Nome do cartão padronizado. Se existir, substitui o 040-000
      fpNomeAdministradora := ALinha.Informacao.AsString;
      fpNFCeSAT.Bandeira := fpNomeAdministradora;
    end;
  else
    inherited ProcessarTipoInterno(ALinha);
  end;
end;

{ TACBrTEFDClass }

constructor TACBrTEFDPayGo.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);

  ArqReq    := CACBrTEFDPayGo_ArqReq;
  ArqResp   := CACBrTEFDPayGo_ArqResp;
  ArqSTS    := CACBrTEFDPayGo_ArqSTS;
  ArqTemp   := CACBrTEFDPayGo_ArqTemp;
  GPExeName := '';
  fpTipo    := gpPayGo;
  Name      := 'PagGo' ;

  fSuportaNSUEstendido := True;
  fSuportaReajusteValor := False;
  fSuportaViasDiferenciadas := True;

  if Assigned(fpResp) then
    fpResp.Free ;

  fpResp := TACBrTEFDRespPayGo.Create;
  fpResp.TipoGP := fpTipo;
end;

procedure TACBrTEFDPayGo.Inicializar;
var
  DirPayGo, DirTefDial: String;
begin
  if Inicializado then exit ;

  // Verificando se Diretório "C:\PAYGO" existe.
  //  Se não existir usa em modo de compatibilidade "C:\TEF_DIAL"
  if (ArqReq = CACBrTEFDPayGo_ArqReq) then
  begin
    DirPayGo := ExtractFileDir(CACBrTEFDPayGo_ArqReq);
    DirTefDial := ExtractFileDir(CACBrTEFDDial_ArqReq);

    if (not DirectoryExists(DirPayGo)) and DirectoryExists(DirTefDial) then
    begin
      ArqReq    := CACBrTEFDDial_ArqReq;
      ArqResp   := CACBrTEFDDial_ArqResp;
      ArqSTS    := CACBrTEFDDial_ArqSTS;
      ArqTemp   := CACBrTEFDDial_ArqTemp;
      GPExeName := CACBrTEFDDial_GPExeName;
    end;
  end;

  inherited Inicializar;
end;

procedure TACBrTEFDPayGo.AdicionarIdentificacao;
var
  Operacoes: Integer ;
begin
  with TACBrTEFD(Owner) do
  begin
    // 733-000 Versão da interface n..3 Valor fixo, identificando a versão deste documento
    // implementada pela Automação Comercial (somente números, por exemplo, 210 para “v2.10”)
    Req.Conteudo.GravaInformacao(733,000, CACBrTEFDPayGoVersaoInterface);

    if (Identificacao.NomeAplicacao  <> '') then
      Req.Conteudo.GravaInformacao(735,000, Identificacao.NomeAplicacao);

    if (Identificacao.VersaoAplicacao <> '') then
      Req.Conteudo.GravaInformacao(736,000, Identificacao.VersaoAplicacao);

    if (Identificacao.RegistroCertificacao <> '') then
      Req.Conteudo.GravaInformacao(738,000, Identificacao.RegistroCertificacao) ;

    if (Identificacao.RazaoSocial <> '') then
      Req.Conteudo.GravaInformacao(716,000, Identificacao.RazaoSocial)
    else if (Identificacao.SoftwareHouse <> '') then
      Req.Conteudo.GravaInformacao(716,000, Identificacao.SoftwareHouse);

    Operacoes := 4;   // 4: valor fixo, sempre incluir

    if AutoEfetuarPagamento then
    begin
      if SuportaSaque and not SuportaDesconto then
        Operacoes := Operacoes + 1
      else if SuportaDesconto and not SuportaSaque then
        Operacoes := Operacoes + 2;
    end
    else
    begin
      if SuportaSaque then
        Operacoes := Operacoes + 1;  // 1: funcionalidade de troco (ver campo 708-000)

      if SuportaDesconto then
        Operacoes := Operacoes + 2;  // 2: funcionalidade de desconto (ver campo 709-000)

      if SuportaViasDiferenciadas then
      begin
        Operacoes := Operacoes + 8;  // 8: vias diferenciadas do comprovante para Cliente/Estabelecimento (campos 712-000 a 715-000)
        if ImprimirViaClienteReduzida then
          Operacoes := Operacoes + 16; // 16: cupom reduzido (campos 710-000 e 711-000)
      end;

      if SuportaReajusteValor then
      begin
        Operacoes := Operacoes + 32 +  // 32: funcionalidade de valor devido (ver campo 743-000)
                                 64 ;  // 64: funcionalidade de valor reajustado (ver campo 744-000)
      end;

      if SuportaNSUEstendido then     // 128: suporta NSU com tamanho de até 40 caracteres (campos 012-000 e 025-000)
        Operacoes := Operacoes + 128;
    end;

    Req.Conteudo.GravaInformacao(706,000, IntToStr(Operacoes) ) ;
  end;
end;

procedure TACBrTEFDPayGo.ProcessarResposta;
begin
  fpSalvarArquivoBackup := Resp.Confirmar;
  inherited ProcessarResposta;
end;

procedure TACBrTEFDPayGo.FinalizarRequisicao;
begin
  VerificarIniciouRequisicao;

  // Se informou a DataHora, vamos informar no campo 717
  if Trim(Req.Conteudo.LeInformacao(22,0).AsString) <> '' then
    if (pos(Req.Header, 'CRT,CHQ,ADM,CNC,CNF,NCN') > 0) then
      Req.GravaInformacao(717,0, FormatDateTime('YYMMDDhhnnss', Req.DataHoraTransacaoComprovante) );

  if (pos(Req.Header, 'CRT,ADM,CNC') > 0) then
    Req.GravaInformacao(725,0, CACBrTEFDPayGoNome+' '+CACBrTEFDPayGoVersao );  // 725-000 Dados adicionais #4

  inherited FinalizarRequisicao;
end;

end.


