{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2024 Daniel Simoes de Almeida               }
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

unit ACBrTEFAPI;

interface

uses
  Classes,
  SysUtils,
  {$IfDef HAS_PNG}
    {$IfDef FMX}
      FMX.Graphics,
    {$Else}
      Graphics,
    {$EndIf}
    ACBrImage, ACBrDelphiZXingQRCode,
    {$IfNDef FPC}
      {$IfNDef FMX}
        pngimage,
      {$EndIf}
      Types,
    {$EndIf}
  {$EndIf}
  ACBrBase,
  ACBrTEFAPIComum;

type
  TACBrTEFAPITipo = ( tefApiNenhum,
                      tefApiPayGoWeb,
                      tefApiCliSiTEF,
                      tefApiElgin,
                      tefStoneAutoTEF,
                      tefAditumAPI,
                      tefScopeAPI,
                      tefDestaxaAPI,
                      tefTPag,
                      tefEquals);

  TACBrTEFAPIExibicaoQRCode = ( qrapiNaoSuportado,
                                qrapiAuto,
                                qrapiExibirPinPad,
                                qrapiExibirAplicacao );

  TACBrTEFAPIOperacaoAPI = ( opapiFluxoAPI,
                             opapiAguardaUsuario,
                             opapiPinPad,
                             opapiPinPadLerCartao,
                             opapiPinPadDigitacao,
                             opapiRemoveCartao,
                             opapiLeituraQRCode );


    TACBrTEFAPITela = ( telaTodas,
                        telaOperador,
                        telaCliente );

    TACBrTEFAPITiposEntrada = ( tedApenasLeitura,
                                tedTodos,
                                tedNumerico,
                                tedAlfabetico,
                                tedAlfaNum );

    TACBrTEFAPIValidacaoDado = ( valdNenhuma,
                                 valdNaoVazio,
                                 valdDigMod10,
                                 valdCPF,
                                 valdCNPJ,
                                 valdCPFouCNPJ,
                                 valdMesAno,
                                 valdDiaMesAno,
                                 valdDuplaDigitacao,
                                 valdSenhaGerente,
                                 valdSenhaLojista,
                                 valdSenhaTecnica,
                                 valdQuantidadeParcelas);

    TACBrTEFAPITipoBarras = ( tbQualquer,
                              tbDigitado,
                              tbLeitor );

    TACBrTEFAPIDadoPinPad = ( dpDDD, dpRedDDD,
                              dpFone, dpRedFone,
                              dpDDDeFone, dpRedDDDeFone,
                              dpCPF, dpRedCPF,
                              dpRG, dpRedRG,
                              dp4UltDigitos,
                              dpCodSeguranca,
                              dpCNPJ, dpRedCNPJ,
                              dpDataDDMMAAAA, dpDataDDMMAA, dpDataDDMM,
                              dpDiaDD, dpMesMM, dpAnoAA, dpAnoAAAA,
                              dpDataNascimentoDDMMAAAA, dpDataNascimentoDDMMAA, dpDataNascimentoDDMM,
                              dpDiaNascimentoDD, dpMesNascimentoMM, dpAnoNascimentoAA, dpAnoNascimentoAAAA,
                              dpIdentificacao,
                              dpCodFidelidade, dpNumeroMesa, dpQtdPessoas, dpQuantidade,
                              dpNumeroBomba, dpNumeroVaga, dpNumeroGuiche,
                              dpCodVendedor, dpCodGarcom, dpNotaAtendimento,
                              dpNumeroNotaFiscal, dpNumeroComanda,
                              dpPlacaVeiculo, dpQuilometragem, dpQuilometragemInicial, dpQuilometragemFinal,
                              dpPorcentagem,
                              dpPesquisaSatisfacao0_10, dpAvalieAtendimento0_10,
                              dpToken, dpNumeroCartao,
                              dpNumeroParcelas,
                              dpCodigoPlano, dpCodigoProduto );

    TACBrTEFAPIImagemPinPad = ( imgJPG, imgPNG );

  TACBrTEFAPIDefinicaoCampo = record
    TituloPergunta: String;
    MascaraDeCaptura: String;
    TipoDeEntrada: TACBrTEFAPITiposEntrada;
    TamanhoMinimo: Integer;
    TamanhoMaximo: Integer;
    ValorMinimo : LongWord;
    ValorMaximo : LongWord;
    OcultarDadosDigitados: Boolean;
    ValidacaoDado: TACBrTEFAPIValidacaoDado;
    ValorInicial: String;
    MsgErroDeValidacao: String;
    MsgErroDadoMaior: String;
    MsgErroDadoMenor: String;
    MsgConfirmacaoDuplaDigitacao: String;
    TipoEntradaCodigoBarras: TACBrTEFAPITipoBarras;
    TipoCampo: Integer;
  end;

  TACBrTEFAPIQuandoEsperarFluxoAPI = procedure(
    OperacaoAPI: TACBrTEFAPIOperacaoAPI; var Cancelar: Boolean)
     of object;

  TACBrTEFAPIQuandoExibirMensagem = procedure(
    const Mensagem: String;
    Terminal: TACBrTEFAPITela;
    MilissegundosExibicao: Integer  // 0 - Para com OK;
    ) of object;                    // Positivo - Aguarda N milissegundos, e apaga a msg;
                                    // Negativo - Apenas exibe a Msg (não aguarda e não apaga msg)

  TACBrTEFAPIQuandoPerguntarMenu = procedure(
    const Titulo: String;
    Opcoes: TStringList;
    var ItemSelecionado: Integer) of object;  // Retorna o Item Selecionado, iniciando com 0
                                              // -2 - Volta no Fluxo
                                              // -1 - Cancela o Fluxo

  TACBrTEFAPIQuandoPerguntarCampo = procedure(
    DefinicaoCampo: TACBrTEFAPIDefinicaoCampo;
    var Resposta: String;
    var Validado: Boolean;
    var Cancelado: Boolean) of object ;

  TACBrTEFAPIQuandoExibirQRCode = procedure(
    const DadosQRCode: String) of object ;

  { TACBrTEFAPIClass }

  TACBrTEFAPIClass = class(TACBrTEFAPIComumClass)
  protected
    procedure FinalizarChamadaAPI; override;
    procedure CalcularTamanhosCampoDadoPinPad( TipoDado: TACBrTEFAPIDadoPinPad;
      out MinLen: SmallInt; out MaxLen: SmallInt);
  public
    procedure ExibirMensagemPinPad(const MsgPinPad: String); virtual;
    function ObterDadoPinPad(TipoDado: TACBrTEFAPIDadoPinPad;
      TimeOut: Integer = 30000; MinLen: SmallInt = 0; MaxLen: SmallInt = 0): String; virtual;
    function MenuPinPad(const Titulo: String; Opcoes: TStrings; TimeOut: Integer = 30000): Integer; virtual;
    function VerificarPresencaPinPad: Byte; virtual;

    procedure ObterListaImagensPinPad(ALista: TStrings); virtual;
    procedure ObterDimensoesVisorPinPad(out Width: Word; out Height: Word); virtual;

    procedure ExibirImagemPinPad(const NomeImagem: String); virtual;
    procedure ApagarImagemPinPad(const NomeImagem: String); virtual;
    procedure CarregarImagemPinPad(const NomeImagem: String; AStream: TStream;
      TipoImagem: TACBrTEFAPIImagemPinPad ); virtual;
    function VersaoAPI: String; virtual;
  end;

  { TACBrTEFAPI }

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(piacbrAllDesktopPlatforms)]
  {$ENDIF RTL230_UP}
  TACBrTEFAPI = class( TACBrTEFAPIComum )
  private
    fTEFModelo: TACBrTEFAPITipo;
    fExibicaoQRCode: TACBrTEFAPIExibicaoQRCode;

    fQuandoExibirQRCode: TACBrTEFAPIQuandoExibirQRCode;
    fQuandoEsperarOperacao: TACBrTEFAPIQuandoEsperarFluxoAPI;
    fQuandoExibirMensagem: TACBrTEFAPIQuandoExibirMensagem;
    fQuandoPerguntarMenu: TACBrTEFAPIQuandoPerguntarMenu;
    fQuandoPerguntarCampo: TACBrTEFAPIQuandoPerguntarCampo;

    function GetTEFAPIClass: TACBrTEFAPIClass;
    procedure SetModelo(const AValue: TACBrTEFAPITipo);
    function GetPathDLL: String;
    procedure SetPathDLL(const Value: String);

    function TratarNomeImagemPinPad(const NomeImagem: String): String;
  protected

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Inicializar; override;

    procedure ExibirMensagemPinPad(const MsgPinPad: String);
    function ObterDadoPinPad(TipoDado: TACBrTEFAPIDadoPinPad;
      TimeOut: Integer = 30000; MinLen: SmallInt = 0; MaxLen: SmallInt = 0): String;
    function MenuPinPad(const Titulo: String; Opcoes: TStrings;
      TimeOut: Integer = 30000): Integer;  // Retorna Opção selecionada, iniciando com 1
    function VerificarPresencaPinPad: Byte;

    procedure ObterListaImagensPinPad(ALista: TStrings);
    procedure ObterDimensoesVisorPinPad(out Width: Word; out Height: Word);

    procedure ExibirImagemPinPad(const NomeImagem: String);
    procedure ExibirQRCodePinPad(const DadosQRCode: String; const NomeImg: String;
      Tamanho: Integer = 0);
    procedure ApagarImagemPinPad(const NomeImagem: String);
    procedure CarregarImagemPinPad(const NomeImagem: String; AStream: TStream;
      TipoImagem: TACBrTEFAPIImagemPinPad ); overload;
    procedure CarregarImagemPinPad(const NomeImagem: String; const Arquivo: String); overload;
    function VersaoAPI: string;

    property TEF: TACBrTEFAPIClass read GetTEFAPIClass;
  published
    property Modelo: TACBrTEFAPITipo
      read fTEFModelo write SetModelo default tefApiNenhum;
    property PathDLL: String read GetPathDLL write SetPathDLL;

    property ExibicaoQRCode: TACBrTEFAPIExibicaoQRCode read fExibicaoQRCode
      write fExibicaoQRCode default qrapiAuto;

    property QuandoEsperarOperacao: TACBrTEFAPIQuandoEsperarFluxoAPI
      read fQuandoEsperarOperacao write fQuandoEsperarOperacao;
    property QuandoExibirMensagem: TACBrTEFAPIQuandoExibirMensagem
      read fQuandoExibirMensagem write fQuandoExibirMensagem;
    property QuandoPerguntarMenu: TACBrTEFAPIQuandoPerguntarMenu
      read fQuandoPerguntarMenu write fQuandoPerguntarMenu;
    property QuandoPerguntarCampo: TACBrTEFAPIQuandoPerguntarCampo
      read fQuandoPerguntarCampo write fQuandoPerguntarCampo;
    property QuandoExibirQRCode: TACBrTEFAPIQuandoExibirQRCode
      read fQuandoExibirQRCode write fQuandoExibirQRCode;
  end;

  {$IfDef HAS_PNG}
    {$IfDef FPC}
      TPngImage = class(TPortableNetworkGraphic);
    {$EndIf}
    {$IfDef FMX}
      TPngImage = TBitmap;  // FMX não tem classe TPngImage e usa PNG por Default
    {$EndIf}
  {$EndIf}


implementation

uses
  TypInfo, Math,
  ACBrUtil.Strings,
  ACBrTEFAPIPayGoWeb,
  ACBrTEFAPICliSiTef,
  ACBrTEFAPIElgin,
  ACBrTEFAPIStoneAutoTEF,
  ACBrTEFAPIAditum,
  ACBrTEFAPIScope,
  ACBrTEFAPIDestaxa,
  ACBrTEFAPITPag,
  ACBrTEFAPIPayKit;

{ TACBrTEFAPIClass }

procedure TACBrTEFAPIClass.FinalizarChamadaAPI;
begin
  ProcessarRespostaOperacaoTEF;
  inherited;
end;

procedure TACBrTEFAPIClass.CalcularTamanhosCampoDadoPinPad(
  TipoDado: TACBrTEFAPIDadoPinPad; out MinLen: SmallInt; out MaxLen: SmallInt);
begin
  case TipoDado of
    dpDDD, dpRedDDD:
      begin
        MinLen := 3; MaxLen := 3;
      end;
    dpFone, dpRedFone:
      begin
        MinLen := 8; MaxLen := 9;
      end;
    dpDDDeFone, dpRedDDDeFone:
      begin
        MinLen := 10; MaxLen := 11;
      end;
    dpCPF, dpRedCPF:
      begin
        MinLen := 11; MaxLen := 11;
      end;
    dpRG, dpRedRG:
      begin
        MinLen := 5; MaxLen := 11;
      end;
    dp4UltDigitos:
      begin
        MinLen := 4; MaxLen := 4;
      end;
    dpCodSeguranca:
      begin
        MinLen := 3; MaxLen := 3;
      end;
    dpCNPJ, dpRedCNPJ:
      begin
        MinLen := 14; MaxLen := 14;
      end;
    dpDataDDMMAAAA, dpDataNascimentoDDMMAAAA:
      begin
        MinLen := 8; MaxLen := 8;
      end;
    dpDataDDMMAA,  dpDataNascimentoDDMMAA:
      begin
        MinLen := 6; MaxLen := 6;
      end;
    dpDataDDMM, dpDataNascimentoDDMM, dpAnoAAAA, dpAnoNascimentoAAAA:
      begin
        MinLen := 4; MaxLen := 4;
      end;
    dpDiaDD, dpMesMM, dpAnoAA, dpDiaNascimentoDD, dpMesNascimentoMM, dpAnoNascimentoAA:
      begin
        MinLen := 2; MaxLen := 2;
      end;
    dpPesquisaSatisfacao0_10, dpAvalieAtendimento0_10, dpNotaAtendimento:
      begin
        MinLen := 1; MaxLen := 2;
      end;
    dpNumeroParcelas:
      begin
        MinLen := 1; MaxLen := 3;
      end;
  end;
end;

procedure TACBrTEFAPIClass.ExibirMensagemPinPad(const MsgPinPad: String);
begin
  ErroAbstract('ExibirMensagemPinPad');
end;

function TACBrTEFAPIClass.ObterDadoPinPad(TipoDado: TACBrTEFAPIDadoPinPad;
  TimeOut: Integer; MinLen: SmallInt; MaxLen: SmallInt): String;
begin
  Result := '';
  ErroAbstract('ObterDadoPinPad');
end;

function TACBrTEFAPIClass.MenuPinPad(const Titulo: String; Opcoes: TStrings;
  TimeOut: Integer): Integer;
begin
  Result := 0;
  ErroAbstract('MenuPinPad');
end;

function TACBrTEFAPIClass.VerificarPresencaPinPad: Byte;
begin
  Result := 0;
  ErroAbstract('VerificarPresencaPinPad');
end;

function TACBrTEFAPIClass.VersaoAPI: String;
begin
  Result := '';
  ErroAbstract('VersaoAPI');
end;

procedure TACBrTEFAPIClass.ObterListaImagensPinPad(ALista: TStrings);
begin
  ErroAbstract('ObterListaImagensPinPad');
end;

procedure TACBrTEFAPIClass.ObterDimensoesVisorPinPad(out Width: Word; out
  Height: Word);
begin
  Width := 150;
  Height := 150;
end;

procedure TACBrTEFAPIClass.ExibirImagemPinPad(const NomeImagem: String);
begin
  ErroAbstract('ExibirImagemPinPad');
end;

procedure TACBrTEFAPIClass.ApagarImagemPinPad(const NomeImagem: String);
begin
  ErroAbstract('ApagarImagemPinPad');
end;

procedure TACBrTEFAPIClass.CarregarImagemPinPad(const NomeImagem: String;
  AStream: TStream; TipoImagem: TACBrTEFAPIImagemPinPad);
begin
  ErroAbstract('CarregarImagemPinPad');
end;

{ TACBrTEFAPI }

constructor TACBrTEFAPI.Create(AOwner: TComponent);
begin
  inherited;
  fQuandoEsperarOperacao := Nil;
  fQuandoExibirMensagem := Nil;
  fQuandoPerguntarMenu := Nil;
  fQuandoPerguntarCampo := Nil;
  fQuandoExibirQRCode := Nil;

  fTEFModelo := tefApiNenhum;
  fExibicaoQRCode := qrapiAuto;
end;

destructor TACBrTEFAPI.Destroy;
begin
  inherited;
end;

procedure TACBrTEFAPI.Inicializar;
begin
  inherited;

  if not Assigned( fQuandoExibirMensagem )  then
    DoException( Format(sACBrTEFAPIEventoInvalidoException, ['QuandoExibirMensagem']));

  if not Assigned( fQuandoPerguntarMenu )  then
    DoException( Format( sACBrTEFAPIEventoInvalidoException, ['QuandoPerguntarMenu']));

  if not Assigned( fQuandoPerguntarCampo )  then
    DoException( Format( sACBrTEFAPIEventoInvalidoException, ['QuandoPerguntarCampo']));

  if not Assigned( fQuandoExibirQRCode ) then
    ExibicaoQRCode := qrapiExibirPinPad;
end;

procedure TACBrTEFAPI.ExibirMensagemPinPad(const MsgPinPad: String);
begin
  GravarLog('ExibirMensagemPinPad( '+MsgPinPad+' )');
  TEF.ExibirMensagemPinPad(MsgPinPad);
end;

function TACBrTEFAPI.ObterDadoPinPad(TipoDado: TACBrTEFAPIDadoPinPad;
  TimeOut: Integer; MinLen: SmallInt; MaxLen: SmallInt): String;
begin
  GravarLog('ObterDadoPinPad( '+
            GetEnumName(TypeInfo(TACBrTEFAPIDadoPinPad), integer(TipoDado) )+', '+
            IntToStr(TimeOut)+', '+IntToStr(MinLen)+', '+IntToStr(MaxLen)+' )');
  Result := TEF.ObterDadoPinPad(TipoDado, TimeOut, MinLen, MaxLen);
  GravarLog('   '+Result);
end;

function TACBrTEFAPI.MenuPinPad(const Titulo: String; Opcoes: TStrings;
  TimeOut: Integer): Integer;
begin
  GravarLog('MenuPinPad( '+Titulo + sLineBreak + Opcoes.Text+' )');
  Result := TEF.MenuPinPad(Titulo, Opcoes, TimeOut);
  GravarLog('   '+IntToStr(Result));
end;

function TACBrTEFAPI.VerificarPresencaPinPad: Byte;
begin
  GravarLog('VerificarPresencaPinPad');
  Result := TEF.VerificarPresencaPinPad;
  GravarLog('   '+IntToStr(Result));
end;

function TACBrTEFAPI.VersaoAPI: string;
var
  lVersaoAPI: string;
begin
  GravarLog('VersaoAPI');
  lVersaoAPI := TEF.VersaoAPI;
  GravarLog(lVersaoAPI);
end;

procedure TACBrTEFAPI.ObterListaImagensPinPad(ALista: TStrings);
begin
  GravarLog('ObterListaImagensPinPad');
  ALista.Clear;
  TEF.ObterListaImagensPinPad(ALista);
  GravarLog(ALista.Text);
end;

procedure TACBrTEFAPI.ObterDimensoesVisorPinPad(out Width: Word; out
  Height: Word);
begin
  GravarLog('ObterDimensoesVisorPinPad');
  TEF.ObterDimensoesVisorPinPad(Width, Height);
  GravarLog('  Width: '+IntToStr(Width)+', Height: '+IntToStr(Height));
end;

procedure TACBrTEFAPI.ExibirImagemPinPad(const NomeImagem: String);
var
  n: String;
begin
  GravarLog('ExibirImagemPinPad( '+NomeImagem+' )');
  n := TratarNomeImagemPinPad(NomeImagem);
  TEF.ExibirImagemPinPad(n);
end;

{$IfDef HAS_PNG}
procedure TACBrTEFAPI.ExibirQRCodePinPad(const DadosQRCode: String;
  const NomeImg: String; Tamanho: Integer);
var
  bmp: TBitmap;
  png: TPngImage;
  ms: TMemoryStream;
  wimg: String;
  {$IfDef FMX}
   r: TRectF;
  {$Else}
   r: TRect;
  {$EndIf}
  w, h: Word;
begin
  if Trim(DadosQRCode) = '' then
  begin
    ExibirMensagemPinPad('');
    Exit;
  end;

  if (Tamanho = 0) then
  begin
    ObterDimensoesVisorPinPad(w, h);
    Tamanho := min(w, h) - 20;
  end;

  wimg := UpperCase(PadRight(Trim(NomeImg), 8, '0'));
  bmp := TBitmap.Create;
  png := TPngImage.Create;
  ms := TMemoryStream.Create;
  try
    PintarQRCode(DadosQRCode, bmp, qrUTF8BOM);
    r.Top := 0;
    r.Left := 0;
    r.Bottom := r.Top + Tamanho;// r.Height := Tamanho; //Delphi 2010 não tem TRect.Height
    r.Right := r.Left + Tamanho;// r.Width := Tamanho; //Delphi 2010 não tem TRect.Width
    png.Assign(bmp);
    {$IfDef FPC}
     png.SetSize(Tamanho, Tamanho);
    {$Else}
     png.ReSize(Tamanho, Tamanho);
    {$EndIf}
    {$IfDef FMX}
     png.Canvas.BeginScene;
     try
       png.Canvas.DrawBitmap(bmp, bmp.BoundsF, r, 1);
     finally
       png.Canvas.EndScene;
     end;
    {$Else}
     png.Canvas.StretchDraw(r, bmp);
    {$EndIf}
    png.SaveToStream(ms);
    ms.Position := 0;
    CarregarImagemPinPad(wimg, ms, imgPNG);
    ExibirImagemPinPad(wimg);
  finally
    bmp.Free;
    png.Free;
    ms.Free;
  end;
end;
{$Else}
procedure TACBrTEFAPI.ExibirQRCodePinPad(const DadosQRCode: String;
  const NomeImg: String; Tamanho: Integer);
begin
  DoException(ACBrStr(sACBrTEFAPIPNGNaoSuportado));
end;
{$EndIf}

procedure TACBrTEFAPI.ApagarImagemPinPad(const NomeImagem: String);
var
  n: String;
begin
  GravarLog('ApagarImagemPinPad( '+NomeImagem+' )');
  n := TratarNomeImagemPinPad(NomeImagem);
  TEF.ApagarImagemPinPad(n);
end;

procedure TACBrTEFAPI.CarregarImagemPinPad(const NomeImagem: String;
  AStream: TStream; TipoImagem: TACBrTEFAPIImagemPinPad);
var
  n: String;
begin
  GravarLog('CarregarImagemPinPad( '+NomeImagem+', '+IntToStr(AStream.Size)+' bytes )');
  n := TratarNomeImagemPinPad(NomeImagem);
  TEF.CarregarImagemPinPad(n, AStream, TipoImagem);
end;

procedure TACBrTEFAPI.CarregarImagemPinPad(const NomeImagem: String;
  const Arquivo: String);
var
  warq, ext: String;
  ms: TMemoryStream;
  imgtype: TACBrTEFAPIImagemPinPad;
begin
  GravarLog('CarregarImagemPinPad( '+NomeImagem+', '+Arquivo+' )');
  warq := Trim(Arquivo);
  if (warq = '') then
    Exit;

  if not FileExists(warq) then
    DoException(ACBrStr(Format(sACBrTEFAPIArquivoNaoExistenteException, [warq])));

  ext := LowerCase(ExtractFileExt(warq));
  if ext = '.png' then
    imgtype := imgPNG
  else
    imgtype := imgJPG;

  ms := TMemoryStream.Create;
  try
    ms.LoadFromFile(warq);
    CarregarImagemPinPad(NomeImagem, ms, imgtype);
  finally
    ms.Free;
  end;
end;

procedure TACBrTEFAPI.SetModelo(const AValue: TACBrTEFAPITipo);
begin
  if fTEFModelo = AValue then
    Exit;

  GravarLog('SetModelo( '+GetEnumName(TypeInfo(TACBrTEFAPITipo), integer(AValue))+' )');

  if Inicializado then
    DoException( Format(sACBrTEFAPIComponenteInicializadoException, ['Modelo']));

  FreeAndNil( fpTEFAPIClass ) ;

  { Instanciando uma nova classe de acordo com AValue }
  case AValue of
    tefApiPayGoWeb : fpTEFAPIClass := TACBrTEFAPIClassPayGoWeb.Create( Self );
    tefApiCliSiTEF : fpTEFAPIClass := TACBrTEFAPIClassCliSiTef.Create( Self );
    tefApiElgin    : fpTEFAPIClass := TACBrTEFAPIClassElgin.Create( Self );
    tefStoneAutoTEF: fpTEFAPIClass := TACBrTEFAPIClassStoneAutoTEF.Create( Self );
    tefAditumAPI   : fpTEFAPIClass := TACBrTEFAPIClassAditum.Create( Self );
    tefScopeAPI    : fpTEFAPIClass := TACBrTEFAPIClassScope.Create( Self );
    tefDestaxaAPI  : fpTEFAPIClass := TACBrTEFAPIClassDestaxa.Create( Self );
    tefTPag        : fpTEFAPIClass := TACBrTEFAPIClassTPag.Create( Self );
    tefEquals      : fpTEFAPIClass := TACBrTEFAPIClassPayKit.Create( Self );
  else
    fpTEFAPIClass := TACBrTEFAPIClass.Create( Self );
  end;

  fTEFModelo := AValue;
  RespostasTEF.LimparRespostasTEF;
  CriarTEFResp;
end;

function TACBrTEFAPI.GetPathDLL: String;
begin
  if Assigned(fpTEFAPIClass) then
    Result := fpTEFAPIClass.PathDLL
  else
    Result := '';
end;

procedure TACBrTEFAPI.SetPathDLL(const Value: String);
begin
  if Assigned(fpTEFAPIClass) then
    fpTEFAPIClass.PathDLL := value;
end;

function TACBrTEFAPI.TratarNomeImagemPinPad(const NomeImagem: String): String;
begin
  Result := UpperCase(PadRight(OnlyAlphaNum(NomeImagem), 8, '0'));
end;

function TACBrTEFAPI.GetTEFAPIClass: TACBrTEFAPIClass;
begin
  Result := TACBrTEFAPIClass(fpTEFAPIClass);
end;

end.
