{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Isaque Pinheiro                                 }
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

{******************************************************************************
|* Historico
|*
|* 10/04/2009: Isaque Pinheiro
|*  - Criação e distribuição da Primeira Versao
|* 04/03/2015: Flavio Rubens Massaro Jr.
|* - Modificação para contemplar layout 3 referente ao ano calendario 2014
|* 16/05/2017: Renato Rubinho
|*  - Criação e distribuição para o layout 5
*******************************************************************************}

unit ACBrSpedContabil;
{$I ACBr.inc}

interface

uses
  SysUtils, Classes, DateUtils,
  {$IFDEF FPC}
    LResources,
  {$ENDIF}
  ACBrBase, ACBrSped, ACBrTXTClass,
  ACBrECDBloco_0_Class, ACBrECDBloco_C_Class, ACBrECDBloco_I_Class,
  ACBrECDBloco_J_Class, ACBrECDBloco_K_Class, ACBrECDBloco_9_Class;

type

  /// ACBrSpedContabil - Sitema Publico de Escrituração Digital Contabil

  { TACBrSPEDContabil }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(piacbrAllPlatforms)]
  {$ENDIF RTL230_UP}
  TACBrSPEDContabil = class(TACBrComponent)
  private
    FACBrTXT: TACBrTXTClass;
    FArquivo: String;
    FInicializado : boolean;
    FOnError: TErrorEvent;
    FWriteManual: Boolean;

    FDT_INI: TDateTime;           /// Data inicial das informações contidas no arquivo
    FDT_FIN: TDateTime;           /// Data final das informações contidas no arquivo

    FPath: String;            /// Path do arquivo a ser gerado
    FDelimitador: String;     /// Caracter delimitador de campos
    FTrimString: boolean;         /// Retorna a string sem espaços em branco iniciais e finais
    FCurMascara: String;      /// Mascara para valores tipo currency

    FBloco_0: TBloco_0;
    FBloco_C: TBloco_C;
    FBloco_9: TBloco_9;
    FBloco_I: TBloco_I;
    FBloco_J: TBloco_J;
    FBloco_K: TBloco_K;
    FReplaceDelimitador: Boolean;
    FDeveAtualizarTotalLinhasNosRegistros: Boolean;

    function GetDelimitador: String;
    function GetReplaceDelimitador: Boolean;
    function GetTrimString: boolean;
    function GetCurMascara: String;
    function GetDT_FIN: TDateTime;
    function GetDT_INI: TDateTime;
    procedure SetDelimitador(const Value: String);
    procedure SetReplaceDelimitador(const Value: Boolean);
    procedure SetTrimString(const Value: boolean);
    procedure SetCurMascara(const Value: String);
    procedure SetDT_FIN(const Value: TDateTime);
    procedure SetDT_INI(const Value: TDateTime);

    function GetOnError: TErrorEvent; /// Método do evento OnError
    procedure SetOnError(const Value: TErrorEvent); /// Método SetError

    procedure LimpaRegistros;
    function GetLinhasBuffer: Integer;
    procedure SetLinhasBuffer(const Value: Integer);

    procedure InicializaBloco(Bloco: TACBrSPED);
    procedure SetArquivo(const Value: String);
    function GetConteudo: TStringList;

    procedure SubstituiMascaraPorQTDLinhasEmArquivoFinal(const Mascara, Texto: string);

  protected
    procedure WriteRegistro0990;
    procedure WriteRegistroC990;
    procedure WriteRegistroI990;
    procedure WriteRegistroJ990;
    procedure WriteRegistroK990;
    procedure WriteRegistro9900;
    procedure WriteRegistro9990;
    procedure WriteRegistro9999;
  public
    constructor Create(AOwner: TComponent); override; /// Create
    destructor Destroy; override; /// Destroy

    procedure SaveFileTXT; /// Método que escreve o arquivo texto no caminho passado como parâmetro

    procedure IniciaGeracao(const pWriteManual: Boolean = False);

    /// BLOCO 0
    procedure WriteBloco_0;
    procedure WriteRegistro0000;
    procedure WriteRegistro0001;
    procedure WriteRegistro0007;
    procedure WriteRegistro0020;
    procedure WriteRegistro0035;
    procedure WriteRegistro0150;
    //procedure WriteRegistro0180;
    /// BLOCO C
    procedure WriteBloco_C;
    procedure WriteRegistroC001;
    procedure WriteRegistroC040;
    // BLOCO I
    procedure WriteBloco_I;
    procedure WriteRegistroI001;
    procedure WriteRegistroI010;
    procedure WriteRegistroI012;
    procedure WriteRegistroI020;
    procedure WriteRegistroI030;
    procedure WriteRegistroI050;
    procedure WriteRegistroI075;
    procedure WriteRegistroI100;
    procedure WriteRegistroI150;
    procedure WriteRegistroI200;
    procedure WriteRegistroI300;
    procedure WriteRegistroI350;
    procedure WriteRegistroI500;
    procedure WriteRegistroI510;
    procedure WriteRegistroI550;
    // BLOCO J
    procedure WriteBloco_J;
    procedure WriteRegistroJ001;
    procedure WriteRegistroJ005;
    //procedure WriteRegistroJ800;
    //procedure WriteRegistroJ801;
    procedure WriteRegistroJ900;
    procedure WriteRegistroJ930;
    procedure WriteRegistroJ932;
  	procedure WriteRegistroJ935;
    // BLOCO K
    procedure WriteBloco_K;
    procedure WriteRegistroK001;
    procedure WriteRegistroK030;
    // BLOCO 9
    procedure WriteBloco_9;
    procedure WriteRegistro9001;
    // TOTAIS
    procedure AtualizarTotalLinhasEmArquivoFinal;

    property Conteudo: TStringList read GetConteudo;

    property DT_INI: TDateTime read GetDT_INI write SetDT_INI;
    property DT_FIN: TDateTime read GetDT_FIN write SetDT_FIN;
    //
    property Bloco_0: TBloco_0 read FBloco_0 write FBloco_0;
    property Bloco_C: TBloco_C read FBloco_C write FBloco_C;
    property Bloco_9: TBloco_9 read FBloco_9 write FBloco_9;
    property Bloco_I: TBloco_I read FBloco_I write FBloco_I;
    property Bloco_J: TBloco_J read FBloco_J write FBloco_J;
    property Bloco_K: TBloco_K read FBloco_K write FBloco_K;
  published
    property Path: String read FPath write FPath;
    ///
    property Delimitador: String read GetDelimitador write SetDelimitador;
    property ReplaceDelimitador: Boolean read GetReplaceDelimitador write SetReplaceDelimitador;
    property TrimString: boolean read GetTrimString write SetTrimString;
    property CurMascara: String read GetCurMascara write SetCurMascara;

    property Arquivo: String read FArquivo write SetArquivo;

    property OnError: TErrorEvent  read GetOnError write SetOnError;

    property LinhasBuffer : Integer read GetLinhasBuffer write SetLinhasBuffer
      default 1000 ;
    property DeveAtualizarTotalLinhasNosRegistros: Boolean read FDeveAtualizarTotalLinhasNosRegistros write FDeveAtualizarTotalLinhasNosRegistros default True;
  end;

implementation

Uses
  StrUtils, Math, Types, ACBrUtil.Strings, ACBrUtil.FilesIO,
  ACBrECDBloco_9, ACBrECDBloco_I, ACBrECDBloco_K ;

resourcestring
  StrComponenteNaoConfigurado = 'Componente não inicializado para gravação. ' +
  'Verifique o metodo IniciaGeracao.';

{$IFNDEF FPC}
 {$R ACBr_SPEDContabil.dcr}
{$ENDIF}

(* TACBrSPEDContabil *)

constructor TACBrSPEDContabil.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FACBrTXT := TACBrTXTClass.Create;
  FACBrTXT.LinhasBuffer := 1000 ;

  FInicializado := False;
  FDeveAtualizarTotalLinhasNosRegistros := True;

  FBloco_0 := TBloco_0.Create;
  FBloco_C := TBloco_C.Create;
  FBloco_I := TBloco_I.Create;
  FBloco_J := TBloco_J.Create;
  FBloco_K := TBloco_K.Create;
  FBloco_9 := TBloco_9.Create;

  FBloco_C.Bloco_0 := FBloco_0;
  FBloco_I.Bloco_0 := FBloco_0;
  FBloco_J.Bloco_0 := FBloco_0;
  FBloco_K.Bloco_0 := FBloco_0;
  FBloco_9.Bloco_0 := FBloco_0;


  FPath := ExtractFilePath( ParamStr(0) );

  Delimitador        := '|';   	 //Não chamamos a variável diretamente pois precisa-se alterar os registros filhos também.
  ReplaceDelimitador := False;   //Não chamamos a variável diretamente pois precisa-se alterar os registros filhos também.
  CurMascara         := '#0.00'; //Não chamamos a variável diretamente pois precisa-se alterar os registros filhos também.
  TrimString         := True;		 //Não chamamos a variável diretamente pois precisa-se alterar os registros filhos também.
end;

destructor TACBrSPEDContabil.Destroy;
begin
  FACBrTXT.Free;

  FBloco_0.Free;
  FBloco_C.Free;
  FBloco_I.Free;
  FBloco_J.Free;
  FBloco_K.Free;
  FBloco_9.Free;
  inherited;
end;

procedure TACBrSPEDContabil.LimpaRegistros;
begin
  FBloco_0.LimpaRegistros;
  FBloco_C.LimpaRegistros;
  FBloco_I.LimpaRegistros;
  FBloco_J.LimpaRegistros;
  FBloco_K.LimpaRegistros;
  FBloco_9.LimpaRegistros;
end;

function TACBrSPEDContabil.GetDelimitador: String;
begin
  Result := FDelimitador;
end;

procedure TACBrSPEDContabil.SetDelimitador(const Value: String);
begin
  FDelimitador := Value;

  FBloco_0.Delimitador := Value;
  FBloco_C.Delimitador := Value;
  FBloco_I.Delimitador := Value;
  FBloco_J.Delimitador := Value;
  FBloco_K.Delimitador := Value;
  FBloco_9.Delimitador := Value;
end;

function TACBrSPEDContabil.GetCurMascara: String;
begin
   Result := FCurMascara;
end;

procedure TACBrSPEDContabil.SetCurMascara(const Value: String);
begin
  FCurMascara := Value;
  //
  FBloco_0.CurMascara := Value;
  FBloco_C.CurMascara := Value;
  FBloco_I.CurMascara := Value;
  FBloco_J.CurMascara := Value;
  FBloco_K.CurMascara := Value;
  FBloco_9.CurMascara := Value;
end;

function TACBrSPEDContabil.GetTrimString: boolean;
begin
   Result := FTrimString;
end;

procedure TACBrSPEDContabil.SetTrimString(const Value: boolean);
begin
  FTrimString := Value;
  //
  FBloco_0.TrimString := Value;
  FBloco_C.TrimString := Value;
  FBloco_I.TrimString := Value;
  FBloco_J.TrimString := Value;
  FBloco_K.TrimString := Value;
  FBloco_9.TrimString := Value;
end;

function TACBrSPEDContabil.GetDT_INI: TDateTime;
begin
   Result := FDT_INI;
end;

procedure TACBrSPEDContabil.SetDT_INI(const Value: TDateTime);
begin
  FDT_INI := Value;
  //
  FBloco_0.DT_INI := Value;
  FBloco_C.DT_INI := Value;
  FBloco_I.DT_INI := Value;
  FBloco_J.DT_INI := Value;
  FBloco_K.DT_INI := Value;
  FBloco_9.DT_INI := Value;
  //
  if Assigned(FBloco_0) then
  begin
     FBloco_0.Registro0000.DT_INI := Value;
  end;
end;

function TACBrSPEDContabil.GetDT_FIN: TDateTime;
begin
   Result := FDT_FIN;
end;

procedure TACBrSPEDContabil.SetDT_FIN(const Value: TDateTime);
begin
  FDT_FIN := Value;
  //
  FBloco_0.DT_FIN := Value;
  FBloco_C.DT_FIN := Value;
  FBloco_I.DT_FIN := Value;
  FBloco_J.DT_FIN := Value;
  FBloco_K.DT_FIN := Value;
  FBloco_9.DT_FIN := Value;
  //
  if Assigned(FBloco_0) then
  begin
     FBloco_0.Registro0000.DT_FIN := Value;
  end;
end;

function TACBrSPEDContabil.GetOnError: TErrorEvent;
begin
   Result := FOnError;
end;

function TACBrSPEDContabil.GetReplaceDelimitador: Boolean;
begin
  Result := FReplaceDelimitador;
end;

procedure TACBrSPEDContabil.SetOnError(const Value: TErrorEvent);
begin
  FOnError := Value;

  FBloco_0.OnError := Value;
  FBloco_C.OnError := Value;
  FBloco_I.OnError := Value;
  FBloco_J.OnError := Value;
  FBloco_K.OnError := Value;
  FBloco_9.OnError := Value;
end;

procedure TACBrSPEDContabil.SetReplaceDelimitador(const Value: Boolean);
begin
  FReplaceDelimitador := Value;

  FBloco_0.ReplaceDelimitador := Value;
  FBloco_C.ReplaceDelimitador := Value;
  FBloco_I.ReplaceDelimitador := Value;
  FBloco_J.ReplaceDelimitador := Value;
  FBloco_K.ReplaceDelimitador := Value;
  FBloco_9.ReplaceDelimitador := Value;
end;

procedure TACBrSPEDContabil.SaveFileTXT;
begin
  try
    IniciaGeracao;

    WriteBloco_0;
    WriteBloco_C;
    WriteBloco_I;
    WriteBloco_J;
    WriteBloco_K;
    WriteBloco_9;

    if DeveAtualizarTotalLinhasNosRegistros then
      AtualizarTotalLinhasEmArquivoFinal;
  finally
    LimpaRegistros;
    FACBrTXT.Conteudo.Clear;
    FWriteManual := False;
    FInicializado := False;
  end;
end;

procedure TACBrSPEDContabil.WriteRegistro0000;
begin

  if not FInicializado then
    raise EACBrSPEDException.Create( ACBrStr(StrComponenteNaoConfigurado));

  Bloco_0.WriteRegistro0000;
  Bloco_9.Registro9900.AddRegistro9900('0000', 1);
end;

procedure TACBrSPEDContabil.WriteRegistro0001;
begin

  if not FInicializado then
    raise EACBrSPEDException.Create( ACBrStr(StrComponenteNaoConfigurado));

  Bloco_0.WriteRegistro0001;
  Bloco_9.Registro9900.AddRegistro9900('0001', 1);
end;

procedure TACBrSPEDContabil.WriteRegistro0007;
begin

  if not FInicializado then
    raise EACBrSPEDException.Create( ACBrStr(StrComponenteNaoConfigurado));

  Bloco_0.WriteRegistro0007;
  Bloco_9.Registro9900.AddRegistro9900('0007', Bloco_0.Registro0007.Count);

  if FWriteManual then
    Bloco_0.Registro0007.Clear;
end;

procedure TACBrSPEDContabil.WriteRegistro0020;
begin

  if not FInicializado then
    raise EACBrSPEDException.Create( ACBrStr(StrComponenteNaoConfigurado));

  Bloco_0.WriteRegistro0020;
  Bloco_9.Registro9900.AddRegistro9900('0020', Bloco_0.Registro0020.Count);

  if FWriteManual then
    Bloco_0.Registro0020.Clear;
end;

procedure TACBrSPEDContabil.WriteRegistro0035;
begin

  if not FInicializado then
    raise EACBrSPEDException.Create( ACBrStr(StrComponenteNaoConfigurado));

  Bloco_0.WriteRegistro0035;
  Bloco_9.Registro9900.AddRegistro9900('0035', Bloco_0.Registro0035.Count);

  if FWriteManual then
    Bloco_0.Registro0035.Clear;
end;

procedure TACBrSPEDContabil.WriteRegistro0150;
begin

  if not FInicializado then
    raise EACBrSPEDException.Create( ACBrStr(StrComponenteNaoConfigurado));

  Bloco_0.WriteRegistro0150;
  Bloco_9.Registro9900.AddRegistro9900('0150', Bloco_0.Registro0150.Count);
  Bloco_9.Registro9900.AddRegistro9900('0180', Bloco_0.Registro0180Count);

  if FWriteManual then
  begin
    Bloco_0.Registro0150.Clear;
    Bloco_0.Registro0180Count := 0;
  end;
end;

{procedure TACBrSPEDContabil.WriteRegistro0180;
begin
  Bloco_0.WriteRegistro0180;
  Bloco_9.Registro9900.AddRegistro9900('0180', Bloco_0.Registro0180.Count);

  if FWriteManual then
    Bloco_0.Registro0180.Clear;
end;}

procedure TACBrSPEDContabil.WriteRegistro0990;
begin
  Bloco_0.WriteRegistro0990;
  Bloco_9.Registro9900.AddRegistro9900('0990', 1);
end;

procedure TACBrSPEDContabil.WriteRegistroC001;
begin

  if not FInicializado then
    raise EACBrSPEDException.Create( ACBrStr(StrComponenteNaoConfigurado));

  Bloco_C.WriteRegistroC001;
  Bloco_9.Registro9900.AddRegistro9900('C001', 1);
end;

procedure TACBrSPEDContabil.WriteRegistroC040;
begin

  if not FInicializado then
    raise EACBrSPEDException.Create( ACBrStr(StrComponenteNaoConfigurado));

  if Bloco_C.RegistroC001.IND_DAD = 0 then
  begin
    Bloco_C.WriteRegistroC040;
    Bloco_9.Registro9900.AddRegistro9900('C040', 1);
  end;

  Bloco_9.Registro9900.AddRegistro9900('C050', Bloco_C.RegistroC050Count);
  Bloco_9.Registro9900.AddRegistro9900('C051', Bloco_C.RegistroC051Count);
  Bloco_9.Registro9900.AddRegistro9900('C052', Bloco_C.RegistroC052Count);
  Bloco_9.Registro9900.AddRegistro9900('C150', Bloco_C.RegistroC150Count);
  Bloco_9.Registro9900.AddRegistro9900('C155', Bloco_C.RegistroC155Count);
  Bloco_9.Registro9900.AddRegistro9900('C600', Bloco_C.RegistroC600Count);
  Bloco_9.Registro9900.AddRegistro9900('C650', Bloco_C.RegistroC650Count);

  if FWriteManual then
  begin
    Bloco_C.RegistroC040.RegistroC050.Clear;
    Bloco_C.RegistroC050Count := 0;
    Bloco_C.RegistroC051Count := 0;
    Bloco_C.RegistroC052Count := 0;
    Bloco_C.RegistroC040.RegistroC150.Clear;
    Bloco_C.RegistroC150Count := 0;
    Bloco_C.RegistroC155Count := 0;
    Bloco_C.RegistroC040.RegistroC600.Clear;
    Bloco_C.RegistroC600Count := 0;
    Bloco_C.RegistroC650Count := 0;
  end;
end;

procedure TACBrSPEDContabil.WriteRegistroC990;
begin
  Bloco_C.WriteRegistroC990;
  Bloco_9.Registro9900.AddRegistro9900('C990', 1);
end;

procedure TACBrSPEDContabil.WriteRegistroI001;
begin

  if not FInicializado then
    raise EACBrSPEDException.Create( ACBrStr(StrComponenteNaoConfigurado));

  Bloco_I.WriteRegistroI001;
  Bloco_9.Registro9900.AddRegistro9900('I001', 1);
end;

procedure TACBrSPEDContabil.WriteRegistroI010;
begin

  if not FInicializado then
    raise EACBrSPEDException.Create( ACBrStr(StrComponenteNaoConfigurado));

  Bloco_I.WriteRegistroI010;
  Bloco_9.Registro9900.AddRegistro9900('I010', 1);
end;

procedure TACBrSPEDContabil.WriteRegistroI012;
begin

  if not FInicializado then
    raise EACBrSPEDException.Create( ACBrStr(StrComponenteNaoConfigurado));

  Bloco_I.WriteRegistroI012;
  Bloco_9.Registro9900.AddRegistro9900('I012', Bloco_I.RegistroI012.Count);
  Bloco_9.Registro9900.AddRegistro9900('I015', Bloco_I.RegistroI015Count);

  if FWriteManual then
  begin
    Bloco_I.RegistroI012.Clear;
    Bloco_I.RegistroI015Count := 0;
  end;
end;

procedure TACBrSPEDContabil.WriteRegistroI020;
begin

  if not FInicializado then
    raise EACBrSPEDException.Create( ACBrStr(StrComponenteNaoConfigurado));

  Bloco_I.WriteRegistroI020;
  Bloco_9.Registro9900.AddRegistro9900('I020', Bloco_I.RegistroI020.Count);

  if FWriteManual then
    Bloco_I.RegistroI020.Clear;
end;

procedure TACBrSPEDContabil.WriteRegistroI030;
begin

  if not FInicializado then
    raise EACBrSPEDException.Create( ACBrStr(StrComponenteNaoConfigurado));

  // Total de linhas do arquivo
  Bloco_I.RegistroI030.QTD_LIN := Bloco_0.Registro0990.QTD_LIN_0
                                + Bloco_I.RegistroI990.QTD_LIN_I
                                + Bloco_J.RegistroJ990.QTD_LIN_J
                                + Bloco_K.RegistroK990.QTD_LIN_K
                                + Bloco_9.Registro9990.QTD_LIN_9;
  Bloco_I.WriteRegistroI030;
  Bloco_9.Registro9900.AddRegistro9900('I030', 1);
end;

procedure TACBrSPEDContabil.WriteRegistroI050;
begin

  if not FInicializado then
    raise EACBrSPEDException.Create( ACBrStr(StrComponenteNaoConfigurado));

  Bloco_I.WriteRegistroI050;
  Bloco_9.Registro9900.AddRegistro9900('I050', Bloco_I.RegistroI050.Count);
  Bloco_9.Registro9900.AddRegistro9900('I051', Bloco_I.RegistroI051Count);
  Bloco_9.Registro9900.AddRegistro9900('I052', Bloco_I.RegistroI052Count);

  if FWriteManual then
  begin
    Bloco_I.RegistroI050.Clear;
    Bloco_I.RegistroI051Count := 0;
    Bloco_I.RegistroI052Count := 0;
  end;
end;

procedure TACBrSPEDContabil.WriteRegistroI075;
begin

  if not FInicializado then
    raise EACBrSPEDException.Create( ACBrStr(StrComponenteNaoConfigurado));

  Bloco_I.WriteRegistroI075;
  Bloco_9.Registro9900.AddRegistro9900('I075', Bloco_I.RegistroI075.Count);

  if FWriteManual then
    Bloco_I.RegistroI075.Clear;
end;

procedure TACBrSPEDContabil.WriteRegistroI100;
begin

  if not FInicializado then
    raise EACBrSPEDException.Create( ACBrStr(StrComponenteNaoConfigurado));

  Bloco_I.WriteRegistroI100;
  Bloco_9.Registro9900.AddRegistro9900('I100', Bloco_I.RegistroI100.Count);

  if FWriteManual then
    Bloco_I.RegistroI100.Clear;
end;

procedure TACBrSPEDContabil.WriteRegistroI150;
begin

  if not FInicializado then
    raise EACBrSPEDException.Create( ACBrStr(StrComponenteNaoConfigurado));

  Bloco_I.WriteRegistroI150;
  Bloco_9.Registro9900.AddRegistro9900('I150', Bloco_I.RegistroI150.Count);
  Bloco_9.Registro9900.AddRegistro9900('I151', Bloco_I.RegistroI151Count);
  Bloco_9.Registro9900.AddRegistro9900('I155', Bloco_I.RegistroI155Count);
  Bloco_9.Registro9900.AddRegistro9900('I157', Bloco_I.RegistroI157Count);

  if FWriteManual then
  begin
    Bloco_I.RegistroI150.Clear;
    Bloco_I.RegistroI151Count := 0;
    Bloco_I.RegistroI155Count := 0;
    Bloco_I.RegistroI157Count := 0;
  end;
end;

procedure TACBrSPEDContabil.WriteRegistroI200;
begin

  if not FInicializado then
    raise EACBrSPEDException.Create( ACBrStr(StrComponenteNaoConfigurado));

  Bloco_I.WriteRegistroI200;
  Bloco_9.Registro9900.AddRegistro9900('I200', Bloco_I.RegistroI200.Count);
  Bloco_9.Registro9900.AddRegistro9900('I250', Bloco_I.RegistroI250Count);

  if FWriteManual then
  begin
    Bloco_I.RegistroI200.Clear;
    Bloco_I.RegistroI250Count := 0;
  end;
end;

procedure TACBrSPEDContabil.WriteRegistroI300;
begin

  if not FInicializado then
    raise EACBrSPEDException.Create( ACBrStr(StrComponenteNaoConfigurado));

  Bloco_I.WriteRegistroI300;
  Bloco_9.Registro9900.AddRegistro9900('I300', Bloco_I.RegistroI300.Count);
  Bloco_9.Registro9900.AddRegistro9900('I310', Bloco_I.RegistroI310Count);

  if FWriteManual then
  begin
    Bloco_I.RegistroI300.Clear;
    Bloco_I.RegistroI310Count := 0;
  end;
end;

procedure TACBrSPEDContabil.WriteRegistroI350;
begin

  if not FInicializado then
    raise EACBrSPEDException.Create( ACBrStr(StrComponenteNaoConfigurado));

  Bloco_I.WriteRegistroI350;
  Bloco_9.Registro9900.AddRegistro9900('I350', Bloco_I.RegistroI350.Count);
  Bloco_9.Registro9900.AddRegistro9900('I355', Bloco_I.RegistroI355Count);

  if FWriteManual then
  begin
    Bloco_I.RegistroI350.Clear;
    Bloco_I.RegistroI355Count := 0;
  end;
end;

procedure TACBrSPEDContabil.WriteRegistroI500;
begin

  if not FInicializado then
    raise EACBrSPEDException.Create( ACBrStr(StrComponenteNaoConfigurado));

  Bloco_I.WriteRegistroI500;
  Bloco_9.Registro9900.AddRegistro9900('I500', Bloco_I.RegistroI500.Count);

  if FWriteManual then
    Bloco_I.RegistroI500.Clear;
end;


procedure TACBrSPEDContabil.WriteRegistroI510;
begin

  if not FInicializado then
    raise EACBrSPEDException.Create( ACBrStr(StrComponenteNaoConfigurado));

  Bloco_I.WriteRegistroI510;
  Bloco_9.Registro9900.AddRegistro9900('I510', Bloco_I.RegistroI510.Count);

  if FWriteManual then
    Bloco_I.RegistroI510.Clear;
end;

procedure TACBrSPEDContabil.WriteRegistroI550;
begin

  if not FInicializado then
    raise EACBrSPEDException.Create( ACBrStr(StrComponenteNaoConfigurado));

  Bloco_I.WriteRegistroI550;
  Bloco_9.Registro9900.AddRegistro9900('I550', Bloco_I.RegistroI550.Count);
  Bloco_9.Registro9900.AddRegistro9900('I555', Bloco_I.RegistroI555Count);

  if FWriteManual then
  begin
    Bloco_I.RegistroI550.Clear;
    Bloco_I.RegistroI555Count := 0;
  end;
end;

procedure TACBrSPEDContabil.WriteRegistroI990;
begin
  Bloco_I.WriteRegistroI990;
  Bloco_9.Registro9900.AddRegistro9900('I990', 1);
end;

procedure TACBrSPEDContabil.WriteRegistroJ001;
begin

  if not FInicializado then
    raise EACBrSPEDException.Create( ACBrStr(StrComponenteNaoConfigurado));

  Bloco_J.WriteRegistroJ001;
  Bloco_9.Registro9900.AddRegistro9900('J001', 1);
end;

procedure TACBrSPEDContabil.WriteRegistroJ005;
begin

  if not FInicializado then
    raise EACBrSPEDException.Create( ACBrStr(StrComponenteNaoConfigurado));

  Bloco_J.WriteRegistroJ005;
  Bloco_9.Registro9900.AddRegistro9900('J005', Bloco_J.RegistroJ005.Count);
  Bloco_9.Registro9900.AddRegistro9900('J100', Bloco_J.RegistroJ100Count);
  Bloco_9.Registro9900.AddRegistro9900('J150', Bloco_J.RegistroJ150Count);
  Bloco_9.Registro9900.AddRegistro9900('J200', Bloco_J.RegistroJ200Count);
  Bloco_9.Registro9900.AddRegistro9900('J210', Bloco_J.RegistroJ210Count);
  Bloco_9.Registro9900.AddRegistro9900('J215', Bloco_J.RegistroJ215Count);
  Bloco_9.Registro9900.AddRegistro9900('J800', Bloco_J.RegistroJ800Count);
  Bloco_9.Registro9900.AddRegistro9900('J801', Bloco_J.RegistroJ801Count);

  if FWriteManual then
  begin
    Bloco_J.RegistroJ005.Clear;
    Bloco_J.RegistroJ100Count := 0;
    Bloco_J.RegistroJ150Count := 0;
    Bloco_J.RegistroJ200Count := 0;
    Bloco_J.RegistroJ210Count := 0;
    Bloco_J.RegistroJ215Count := 0;
    Bloco_J.RegistroJ800.Clear;
    Bloco_J.RegistroJ800Count := 0;
    Bloco_J.RegistroJ801.Clear;
    Bloco_J.RegistroJ801Count := 0;
    Bloco_J.RegistroJ930.Clear;
    Bloco_J.RegistroJ932.Clear;
    Bloco_J.RegistroJ935.Clear;
  end;
end;

{procedure TACBrSPEDContabil.WriteRegistroJ800;
begin
  Bloco_J.WriteRegistroJ800;
  AddRegistro9900('J800', Bloco_J.RegistroJ800.Count);

  if FWriteManual then
    Bloco_J.RegistroJ800.Clear;
end;

procedure TACBrSPEDContabil.WriteRegistroJ801;
begin
  Bloco_J.WriteRegistroJ801;
  AddRegistro9900('J801', Bloco_J.RegistroJ801.Count);

  if FWriteManual then
    Bloco_J.RegistroJ801.Clear;
end;}

procedure TACBrSPEDContabil.WriteRegistroJ900;
begin

  if not FInicializado then
    raise EACBrSPEDException.Create( ACBrStr(StrComponenteNaoConfigurado));

  Bloco_J.WriteRegistroJ900;
  Bloco_9.Registro9900.AddRegistro9900('J900', 1);
end;

procedure TACBrSPEDContabil.WriteRegistroJ930;
begin

  if not FInicializado then
    raise EACBrSPEDException.Create( ACBrStr(StrComponenteNaoConfigurado));

  Bloco_J.WriteRegistroJ930;
  Bloco_9.Registro9900.AddRegistro9900('J930', Bloco_J.RegistroJ930.Count);

  if FWriteManual then
    Bloco_J.RegistroJ930.Clear;
end;

procedure TACBrSPEDContabil.WriteRegistroJ932;
begin

  if not FInicializado then
    raise EACBrSPEDException.Create( ACBrStr(StrComponenteNaoConfigurado));

  Bloco_J.WriteRegistroJ932;
  Bloco_9.Registro9900.AddRegistro9900('J932', Bloco_J.RegistroJ932.Count);

  if FWriteManual then
    Bloco_J.RegistroJ932.Clear;
end;

procedure TACBrSPEDContabil.WriteRegistroJ935;
begin

  if not FInicializado then
    raise EACBrSPEDException.Create( ACBrStr(StrComponenteNaoConfigurado));

  Bloco_J.WriteRegistroJ935;
  Bloco_9.Registro9900.AddRegistro9900('J935', Bloco_J.RegistroJ935.Count);

  if FWriteManual then
    Bloco_J.RegistroJ935.Clear;
end;

procedure TACBrSPEDContabil.WriteRegistroJ990;
begin
  Bloco_J.WriteRegistroJ990;
  Bloco_9.Registro9900.AddRegistro9900('J990', 1);
end;

procedure TACBrSPEDContabil.WriteRegistroK001;
begin

  if not FInicializado then
    raise EACBrSPEDException.Create( ACBrStr(StrComponenteNaoConfigurado));

  if ((DT_INI >= EncodeDate(2016, 1, 1)) and (Bloco_0.Registro0000.IND_ESC_CONS = 'S')) then
    if ((Bloco_K.RegistroK001.IND_DAD = 0) or (DT_INI >= EncodeDate(2017, 1, 1))) then
    begin
      Bloco_K.WriteRegistroK001;
      Bloco_9.Registro9900.AddRegistro9900('K001', 1);
    end;
end;

procedure TACBrSPEDContabil.WriteRegistroK030;
begin

  if not FInicializado then
    raise EACBrSPEDException.Create( ACBrStr(StrComponenteNaoConfigurado));

  if (Bloco_K.RegistroK001.IND_DAD = 0) then
  begin
    Bloco_K.WriteRegistroK030;
    Bloco_9.Registro9900.AddRegistro9900('K030', 1);
  end;

  Bloco_9.Registro9900.AddRegistro9900('K100', Bloco_K.RegistroK100Count);
  Bloco_9.Registro9900.AddRegistro9900('K110', Bloco_K.RegistroK110Count);
  Bloco_9.Registro9900.AddRegistro9900('K115', Bloco_K.RegistroK115Count);
  Bloco_9.Registro9900.AddRegistro9900('K200', Bloco_K.RegistroK200Count);
  Bloco_9.Registro9900.AddRegistro9900('K210', Bloco_K.RegistroK210Count);
  Bloco_9.Registro9900.AddRegistro9900('K300', Bloco_K.RegistroK300Count);
  Bloco_9.Registro9900.AddRegistro9900('K310', Bloco_K.RegistroK310Count);
  Bloco_9.Registro9900.AddRegistro9900('K315', Bloco_K.RegistroK315Count);

  if FWriteManual then
  begin
    Bloco_K.RegistroK030.RegistroK100.Clear;
    Bloco_K.RegistroK100Count := 0;
    Bloco_K.RegistroK110Count := 0;
    Bloco_K.RegistroK115Count := 0;
    Bloco_K.RegistroK030.RegistroK200.Clear;
    Bloco_K.RegistroK200Count := 0;
    Bloco_K.RegistroK210Count := 0;
    Bloco_K.RegistroK030.RegistroK300.Clear;
    Bloco_K.RegistroK300Count := 0;
    Bloco_K.RegistroK310Count := 0;
    Bloco_K.RegistroK315Count := 0;
  end;
end;

procedure TACBrSPEDContabil.WriteRegistroK990;
begin
  if ((DT_INI >= EncodeDate(2016, 1, 1)) and (Bloco_0.Registro0000.IND_ESC_CONS = 'S')) then
    if ((Bloco_K.RegistroK001.IND_DAD = 0) or (DT_INI >= EncodeDate(2017, 1, 1))) then
    begin
      Bloco_K.WriteRegistroK990;
      Bloco_9.Registro9900.AddRegistro9900('K990', 1);
    end;
end;

procedure TACBrSPEDContabil.WriteRegistro9001;
begin

  if not FInicializado then
    raise EACBrSPEDException.Create( ACBrStr(StrComponenteNaoConfigurado));

  Bloco_9.Registro9900.AddRegistro9900('9001', 1);
  Bloco_9.WriteRegistro9001;
end;

procedure TACBrSPEDContabil.WriteRegistro9900;
begin
  Bloco_9.Registro9900.AddRegistro9900('9900', Bloco_9.Registro9900.Count + 3);
  Bloco_9.Registro9900.AddRegistro9900('9990', 1);
  Bloco_9.Registro9900.AddRegistro9900('9999', 1);
  Bloco_9.WriteRegistro9900;
end;

procedure TACBrSPEDContabil.WriteRegistro9990;
begin
  Bloco_9.WriteRegistro9990;
end;

procedure TACBrSPEDContabil.WriteRegistro9999;
begin
  Bloco_9.Registro9999.QTD_LIN := Bloco_9.Registro9999.QTD_LIN
                                + Bloco_0.Registro0990.QTD_LIN_0
                                + Bloco_C.RegistroC990.QTD_LIN_C
                                + Bloco_I.RegistroI990.QTD_LIN_I
                                + Bloco_J.RegistroJ990.QTD_LIN_J
                                + Bloco_K.RegistroK990.QTD_LIN_K
                                + Bloco_9.Registro9990.QTD_LIN_9;
  Bloco_9.WriteRegistro9999;
end;
function TACBrSPEDContabil.GetLinhasBuffer: Integer;
begin
   Result := FACBrTXT.LinhasBuffer ;
end;

procedure TACBrSPEDContabil.SetLinhasBuffer(const Value: Integer);
begin
   FACBrTXT.LinhasBuffer := Value ;
end;

procedure TACBrSPEDContabil.InicializaBloco(Bloco: TACBrSPED);
begin
   Bloco.NomeArquivo  := FACBrTXT.NomeArquivo;
   Bloco.LinhasBuffer := FACBrTXT.LinhasBuffer;
   Bloco.Gravado      := False ;
   if not Assigned(Bloco.Conteudo) then
     Bloco.Conteudo := TStringList.Create;
   Bloco.Conteudo.Clear;
end;

procedure TACBrSPEDContabil.IniciaGeracao(const pWriteManual: Boolean = False);
var
  intFor: integer;
begin
  if FInicializado then exit;

  if (Trim(Arquivo) = '') or (Trim(fPath) = '') then
     raise Exception.Create( ACBrStr('Caminho ou nome do arquivo não informado!'));

  FACBrTXT.NomeArquivo := FPath + Arquivo ;
  FACBrTXT.Reset;    // Apaga o Arquivo e limpa memória

  InicializaBloco( Bloco_0 ) ;
  InicializaBloco( Bloco_C ) ;
  InicializaBloco( Bloco_I ) ;
  InicializaBloco( Bloco_J ) ;
  InicializaBloco( Bloco_K ) ;
  InicializaBloco( Bloco_9 ) ;

  /// Preparação para totalizações de registros.
  Bloco_0.Registro0990.QTD_LIN_0 := 0;
  Bloco_C.RegistroC990.QTD_LIN_C := 0;
  Bloco_I.RegistroI990.QTD_LIN_I := 0;
  Bloco_J.RegistroJ990.QTD_LIN_J := 0;
  Bloco_K.RegistroK990.QTD_LIN_K := 0;
  Bloco_9.Registro9990.QTD_LIN_9 := 0;
  Bloco_9.Registro9999.QTD_LIN   := 0;

  for intFor := 0 to Bloco_9.Registro9900.Count - 1 do
  begin
     Bloco_9.Registro9900.Items[intFor] := nil;
     Bloco_9.Registro9900.Items[intFor].Free;
  end;

  Bloco_9.Registro9900.Clear;

  FWriteManual := pWriteManual;
  FInicializado := True;
end;

procedure TACBrSPEDContabil.WriteBloco_0;
begin
  if Bloco_0.Gravado then exit ;

  if not FInicializado then
     raise Exception.Create( 'Métodos "IniciaGeracao" não foi executado' );

  if not(FWriteManual) then
  begin
    WriteRegistro0000;
    WriteRegistro0001;
    WriteRegistro0007;
    WriteRegistro0035;
    WriteRegistro0020;
    WriteRegistro0150;
    //WriteRegistro0180;
  end;

  WriteRegistro0990;

  Bloco_0.WriteBuffer;
  Bloco_0.Conteudo.Clear;
  Bloco_0.Gravado := true;
end;

procedure TACBrSPEDContabil.SetArquivo(const Value: String);
begin
  FArquivo := Value;
end;

procedure TACBrSPEDContabil.WriteBloco_I;
begin
  if Bloco_I.Gravado then Exit;

  if not Bloco_I.Gravado then
    WriteBloco_0;

  if not(FWriteManual) then
  begin
    WriteRegistroI001;
    WriteRegistroI010;
    WriteRegistroI012;
    WriteRegistroI020;
    WriteRegistroI030;
    WriteRegistroI050;
    WriteRegistroI075;
    WriteRegistroI100;
    WriteRegistroI150;
    WriteRegistroI200;
    WriteRegistroI300;
    WriteRegistroI350;
    WriteRegistroI500;
    WriteRegistroI510;
    WriteRegistroI550;
  end;

  WriteRegistroI990;

  Bloco_I.WriteBuffer;
  Bloco_I.Conteudo.Clear;
  Bloco_I.Gravado := true;
end;

procedure TACBrSPEDContabil.WriteBloco_J;
begin
  if Bloco_J.Gravado then Exit;

  if not Bloco_J.Gravado then
    WriteBloco_I;

  if not(FWriteManual) then
  begin
    WriteRegistroJ001;
    WriteRegistroJ005;
    //WriteRegistroJ800;
    //WriteRegistroJ801;
    WriteRegistroJ900;
    WriteRegistroJ930;
    WriteRegistroJ935;
  end;

  WriteRegistroJ990;

  Bloco_J.WriteBuffer;
  Bloco_J.Conteudo.Clear;
  Bloco_J.Gravado := true;
end;

procedure TACBrSPEDContabil.WriteBloco_K;
begin
  if Bloco_K.Gravado then Exit;

  if not Bloco_K.Gravado then
    WriteBloco_J;

  if not(FWriteManual) then
  begin
    WriteRegistroK001;
    WriteRegistroK030;
  end;

  WriteRegistroK990;

  Bloco_K.WriteBuffer;
  Bloco_K.Conteudo.Clear;
  Bloco_K.Gravado := true;
end;

procedure TACBrSPEDContabil.WriteBloco_9;
begin
  if Bloco_9.Gravado then Exit;

  if not Bloco_9.Gravado then
    WriteBloco_K;

  if not(FWriteManual) then
    WriteRegistro9001;

  WriteRegistro9900;
  WriteRegistro9990;
  WriteRegistro9999;

  Bloco_9.WriteBuffer;
  Bloco_9.Conteudo.Clear;
  Bloco_9.Gravado := true;
end;

procedure TACBrSPEDContabil.WriteBloco_C;
begin
  if Bloco_C.Gravado then Exit;

  if not Bloco_C.Gravado then
    WriteBloco_0;

  if Bloco_C.GerarBlocoC then
  begin
    if not(FWriteManual) then
    begin
      WriteRegistroC001;
      WriteRegistroC040;
    end;

    WriteRegistroC990;

    Bloco_C.WriteBuffer;
    Bloco_C.Conteudo.Clear;
  end;

  Bloco_C.Gravado := true;
end;

function TACBrSPEDContabil.GetConteudo: TStringList;
begin
  Result := FACBrTXT.Conteudo;
end;

procedure TACBrSPEDContabil.AtualizarTotalLinhasEmArquivoFinal;
var
  sTotal: String;
begin
  sTotal := FACBrTXT.LFill(Bloco_9.Registro9999.QTD_LIN, 9, False);

  SubstituiMascaraPorQTDLinhasEmArquivoFinal(sIndicadorQTDLinhasArquivo, sTotal);
end;

procedure TACBrSPEDContabil.SubstituiMascaraPorQTDLinhasEmArquivoFinal(const Mascara, Texto: string);
var
  fs: TFileStream;
  sByte, sByteNew: Byte;
  iInc, QtdSubstituicoes, iCont, iIni : Integer;
  sChar : String;
  TamanhoMascara: Integer;
begin
  sChar := '';
  iCont := 0;
  TamanhoMascara := Length(Mascara);

  fs := TFileStream.Create(FACBrTXT.NomeArquivo, fmOpenReadWrite or fmShareExclusive );

//******************************************************************************
// QtdSubstituicoes : soma a quantidade de vezes que foi substituido a mascara
// dentro do arquivo. Para Sped Contabil isso é 2 vezes (Registros J e I).
// Assim, encerra a alteração dos totalizadores de linhas do arquivo e encerra o laço
// de repetiçãoapós duas vezes QtdSubstituicoes = 2
//******************************************************************************
  try
    QtdSubstituicoes := 0;
    while QtdSubstituicoes <> 2 do
    begin
      fs.Position := iCont;
      fs.Read(sByte, 1);
      if (Chr(sByte) = '[') then
      begin
        fs.Position := iCont;
        iIni := iCont;
        sChar := '';

        for iInc := 1 to TamanhoMascara do
        begin
          fs.Position := iCont;
          fs.Read(sByte, 1);
          sChar := sChar + Chr(sByte);
          Inc(iCont);
        end;

        if (sChar = Mascara) then
        begin
          for iInc := 1 to TamanhoMascara do
          begin
            fs.Position := iIni;
            sByteNew := Ord(Texto[iInc]);
            fs.Write(sByteNew,1);
            Inc(iIni);
          end;
          Inc(QtdSubstituicoes);
        end;
      end;
      Inc(iCont);
    end;
  finally
    FreeAndNil(fs);
  end;
end;

{$ifdef FPC}
initialization
   {$I ACBrSpedContabil.lrs}
{$endif}

end.
