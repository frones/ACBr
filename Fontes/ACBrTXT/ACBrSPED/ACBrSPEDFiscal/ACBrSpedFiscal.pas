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
|* 15/03/2010: Alessandro Yamasaki
|*  - Adicionado o REGISTRO 0500: PLANO DE CONTAS CONTÁBEIS
*******************************************************************************}

unit ACBrSpedFiscal;
{$I ACBr.inc}

interface

uses
  SysUtils, Math, Classes, ACBrBase,
{$IFNDEF Framework}
  {$IFDEF FPC}
    LResources,
  {$ENDIF}
{$ENDIF}
  DateUtils, ACBrSped, ACBrTXTClass, ACBrEFDBlocos,
  ACBrEFDBloco_0_Class, ACBrEFDBloco_1_Class, ACBrEFDBloco_9_Class,
  ACBrEFDBloco_B_Class, ACBrEFDBloco_C_Class,  ACBrEFDBloco_D_Class,
  ACBrEFDBloco_E_Class, ACBrEFDBloco_G_Class, ACBrEFDBloco_H_Class,
  ACBrEFDBloco_K_Class, ACBrEFDBloco_0_Events, ACBrEFDBloco_B_Events,
  ACBrEFDBloco_C_Events,ACBrEFDBloco_D_Events, ACBrEFDBloco_E_Events;

type
  /// ACBrSpedFiscal - Sitema Publico de Escrituração Digital Fiscal

  { TACBrSPEDFiscal }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(piacbrAllPlatforms)]
  {$ENDIF RTL230_UP}
  TACBrSPEDFiscal = class(TACBrComponent)
  private
    FACBrTXT: TACBrTXTClass;
    FArquivo: String;
    FInicializado : boolean;
    FOnError: TErrorEvent;

    FChecksBloco_C: TChecksBloco_C;

    FEventsBloco_0: TEventsBloco_0;
    FEventsBloco_B: TEventsBloco_B;
    FEventsBloco_C: TEventsBloco_C;
    FEventsBloco_D: TEventsBloco_D;
    FEventsBloco_E: TEventsBloco_E;

    FDT_INI: TDateTime;           /// Data inicial das informações contidas no arquivo
    FDT_FIN: TDateTime;           /// Data final das informações contidas no arquivo

    FPath: String;            /// Path do arquivo a ser gerado
    FDelimitador: String;     /// Caracter delimitador de campos
    FReplaceDelimitador: Boolean;
    FTrimString: boolean;
    /// Retorna a string sem espaços em branco iniciais e finais
    FCurMascara: String;      /// Mascara para valores tipo currency

    FBloco_0: TBloco_0;
    FBloco_1: TBloco_1;
    FBloco_9: TBloco_9;
    FBloco_B: TBloco_B;
    FBloco_C: TBloco_C;
    FBloco_D: TBloco_D;
    FBloco_E: TBloco_E;
    FBloco_G: TBloco_G;
    FBloco_H: TBloco_H;
    FBloco_K: TBloco_K;

    function GetConteudo: TStringList;
    function GetDelimitador: String;
    function GetReplaceDelimitador: Boolean;
    function GetLinhasBuffer: Integer;
    function GetTrimString: boolean;
    function GetCurMascara: String;
    function GetDT_FIN: TDateTime;
    function GetDT_INI: TDateTime;
    procedure InicializaBloco(Bloco: TACBrSPED);
    procedure SetArquivo(const Value: String);
    procedure SetDelimitador(const Value: String);
    procedure SetReplaceDelimitador(const Value: Boolean);
    procedure SetLinhasBuffer(const Value: Integer);
    procedure SetPath(const Value: String);
    procedure SetTrimString(const Value: boolean);
    procedure SetCurMascara(const Value: String);
    procedure SetDT_FIN(const Value: TDateTime);
    procedure SetDT_INI(const Value: TDateTime);

    function GetOnError: TErrorEvent; /// Método do evento OnError
    procedure SetOnError(const Value: TErrorEvent);/// Método SetError

  protected
    /// BLOCO 0
    procedure WriteRegistro0000;
    procedure WriteRegistro0001;
    procedure WriteRegistro0990;
    /// BLOCO 1
    procedure WriteRegistro1001;
    procedure WriteRegistro1990;
    /// BLOCO B
    procedure WriteRegistroB001;
    procedure WriteRegistroB990;
    /// BLOCO C
    procedure WriteRegistroC001;
    procedure WriteRegistroC990;
    /// BLOCO D
    procedure WriteRegistroD001;
    procedure WriteRegistroD990;
    /// BLOCO E
    procedure WriteRegistroE001;
    procedure WriteRegistroE990;
    /// BLOCO G
    procedure WriteRegistroG001;
    procedure WriteRegistroG990;
    /// BLOCO H
    procedure WriteRegistroH001;
    procedure WriteRegistroH990;
    /// BLOCO K
    procedure WriteRegistroK001;
    procedure WriteRegistroK990;
    /// BLOCO 9
    procedure WriteRegistro9001;
    procedure WriteRegistro9900;
    procedure WriteRegistro9990;
    procedure WriteRegistro9999;

    procedure LimpaRegistros;
  public
    constructor Create(AOwner: TComponent); override; /// Create
    destructor Destroy; override; /// Destroy
    procedure SaveFileTXT;

    procedure IniciaGeracao;
    procedure CancelaGeracao;
    procedure WriteBloco_0;
    procedure WriteBloco_B;
    procedure WriteBloco_C( FechaBloco: Boolean );
    procedure WriteBloco_D;
    procedure WriteBloco_E;
    procedure WriteBloco_G;
    procedure WriteBloco_H;
    procedure WriteBloco_K;
    procedure WriteBloco_1;
    procedure WriteBloco_9;

    property Conteudo: TStringList read GetConteudo;

    property DT_INI: TDateTime read GetDT_INI write SetDT_INI;
    property DT_FIN: TDateTime read GetDT_FIN write SetDT_FIN;

    property Bloco_0: TBloco_0 read FBloco_0 write FBloco_0;
    property Bloco_1: TBloco_1 read FBloco_1 write FBloco_1;
    property Bloco_9: TBloco_9 read FBloco_9 write FBloco_9;
    property Bloco_B: TBloco_B read FBloco_B write FBloco_B;
    property Bloco_C: TBloco_C read FBloco_C write FBloco_C;
    property Bloco_D: TBloco_D read FBloco_D write FBloco_D;
    property Bloco_E: TBloco_E read FBloco_E write FBloco_E;
    property Bloco_G: TBloco_G read FBloco_G write FBloco_G;
    property Bloco_H: TBloco_H read FBloco_H write FBloco_H;
    property Bloco_K: TBloco_K read FBloco_K write FBloco_K;
  published
    property Path: String read FPath write SetPath;
    property Arquivo: String read FArquivo write SetArquivo;
    property LinhasBuffer : Integer read GetLinhasBuffer write SetLinhasBuffer default 1000 ;

    property Delimitador: String read GetDelimitador write SetDelimitador;
    property ReplaceDelimitador: Boolean read GetReplaceDelimitador write SetReplaceDelimitador;
    property TrimString: boolean read GetTrimString write SetTrimString;
    property CurMascara: String read GetCurMascara write SetCurMascara;

    property OnError: TErrorEvent read GetOnError write SetOnError;

    property ChecksBloco_C: TChecksBloco_C read FChecksBloco_C; // write FOnChecksBloco_C;

    property EventsBloco_0: TEventsBloco_0 read FEventsBloco_0; // write FOnEventsBloco_0;
    property EventsBloco_B: TEventsBloco_B read FEventsBloco_B; // write FOnEventsBloco_B;
    property EventsBloco_C: TEventsBloco_C read FEventsBloco_C; // write FOnEventsBloco_C;
    property EventsBloco_D: TEventsBloco_D read FEventsBloco_D; // write FOnEventsBloco_D;
    property EventsBloco_E: TEventsBloco_E read FEventsBloco_E; // write FOnEventsBloco_E;
  end;

implementation

uses ACBrUtil.Base, ACBrUtil.FilesIO, ACBrUtil.Strings;

{$IFNDEF FPC}
 {$R ACBr_SPEDFiscal.dcr}
{$ENDIF}

(* TACBrSPEDFiscal *)

procedure TACBrSPEDFiscal.CancelaGeracao;
begin
  LimpaRegistros;
  FInicializado := False;
end;

constructor TACBrSPEDFiscal.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FACBrTXT := TACBrTXTClass.Create;
  FACBrTXT.LinhasBuffer := 1000 ;

  FInicializado := False;

  FBloco_0 := TBloco_0.Create;
  FBloco_1 := TBloco_1.Create;
  FBloco_B := TBloco_B.Create;
  FBloco_C := TBloco_C.Create(Self);
  FBloco_D := TBloco_D.Create;
  FBloco_E := TBloco_E.Create;
  FBloco_G := TBloco_G.Create;
  FBloco_H := TBloco_H.Create;
  FBloco_K := TBloco_K.Create;
  FBloco_9 := TBloco_9.Create;

  /// Objeto passado por referência para que possamos usa-lo para fazer pesquisa
  /// em seus registros.
  /// Ex: Do Bloco_C registro C425, pesquisar o Bloco_0 registro 0200.
  FBloco_1.Bloco_0 := FBloco_0;
  FBloco_B.Bloco_0 := FBloco_0;
  FBloco_C.Bloco_0 := FBloco_0;
  FBloco_D.Bloco_0 := FBloco_0;
  FBloco_E.Bloco_0 := FBloco_0;
  FBloco_G.Bloco_0 := FBloco_0;
  FBloco_H.Bloco_0 := FBloco_0;
  FBloco_K.Bloco_0 := FBloco_0;

  FPath              := ExtractFilePath(ParamStr(0));
  Delimitador        := '|';  	  //Não chamamos a variável diretamente pois precisa-se alterar os registros filhos também.
  ReplaceDelimitador := False; 	  //Não chamamos a variável diretamente pois precisa-se alterar os registros filhos também.
  CurMascara         := '#0.00';	//Não chamamos a variável diretamente pois precisa-se alterar os registros filhos também.
  TrimString         := True;   	//Não chamamos a variável diretamente pois precisa-se alterar os registros filhos também.

  FEventsBloco_0 := TEventsBloco_0.Create(Self);
  FEventsBloco_0.Name := 'EventsBloco_0';
  FEventsBloco_0.SetSubComponent(True);

  FEventsBloco_B := TEventsBloco_B.Create(Self);
  FEventsBloco_B.Name := 'EventsBloco_B';
  FEventsBloco_B.SetSubComponent(True);

  FEventsBloco_C := TEventsBloco_C.Create(Self);
  FEventsBloco_C.Name := 'EventsBloco_C';
  FEventsBloco_C.SetSubComponent(True);

  FChecksBloco_C := TChecksBloco_C.Create(Self);
  FChecksBloco_C.Name := 'ChecksBloco_C';
  FChecksBloco_C.SetSubComponent(True);

  FEventsBloco_D := TEventsBloco_D.Create(Self);
  FEventsBloco_D.Name := 'EventsBloco_D';
  FEventsBloco_D.SetSubComponent(True);

  FEventsBloco_E := TEventsBloco_E.Create(Self);
  FEventsBloco_E.Name := 'EventsBloco_E';
  FEventsBloco_E.SetSubComponent(True);
end;

destructor TACBrSPEDFiscal.Destroy;
begin
  FACBrTXT.Free;

  FEventsBloco_0.Free;
  FEventsBloco_B.Free;
  FEventsBloco_C.Free;
  FChecksBloco_C.Free;
  FEventsBloco_D.Free;
  FEventsBloco_E.Free;

  FBloco_0.Free;
  FBloco_1.Free;
  FBloco_B.Free;
  FBloco_C.Free;
  FBloco_D.Free;
  FBloco_E.Free;
  FBloco_G.Free;
  FBloco_H.Free;
  FBloco_K.Free;
  FBloco_9.Free;
  inherited;
end;

procedure TACBrSPEDFiscal.LimpaRegistros;
begin
  FBloco_0.LimpaRegistros;
  FBloco_1.LimpaRegistros;
  FBloco_B.LimpaRegistros;
  FBloco_C.LimpaRegistros;
  FBloco_D.LimpaRegistros;
  FBloco_E.LimpaRegistros;
  FBloco_G.LimpaRegistros;
  FBloco_H.LimpaRegistros;
  FBloco_K.LimpaRegistros;
  FBloco_9.LimpaRegistros;
end;

function TACBrSPEDFiscal.GetConteudo: TStringList;
begin
  Result := FACBrTXT.Conteudo;
end;

function TACBrSPEDFiscal.GetDelimitador: String;
begin
  Result := FDelimitador;
end;

function TACBrSPEDFiscal.GetLinhasBuffer: Integer;
begin
   Result := FACBrTXT.LinhasBuffer ;
end;

procedure TACBrSPEDFiscal.SetDelimitador(const Value: String);
begin
  if Value = '' then
     raise EACBrSPEDFiscalException.Create('Campo não pode ser vazio!');

  FDelimitador := Value;

  FBloco_0.Delimitador := Value;
  FBloco_1.Delimitador := Value;
  FBloco_B.Delimitador := Value;
  FBloco_C.Delimitador := Value;
  FBloco_D.Delimitador := Value;
  FBloco_E.Delimitador := Value;
  FBloco_G.Delimitador := Value;
  FBloco_H.Delimitador := Value;
  FBloco_K.Delimitador := Value;
  FBloco_9.Delimitador := Value;
end;

procedure TACBrSPEDFiscal.SetLinhasBuffer(const Value: Integer);
begin
   FACBrTXT.LinhasBuffer := Value ;
end;

procedure TACBrSPEDFiscal.SetPath(const Value: String);
begin
  if Value = '' then
     raise EACBrSPEDFiscalException.Create('Campo não pode ser vazio!');

  FPath := PathWithDelim( Value );
end;

procedure TACBrSPEDFiscal.SetReplaceDelimitador(const Value: Boolean);
begin
  FReplaceDelimitador := Value;

  FBloco_0.ReplaceDelimitador := Value;
  FBloco_1.ReplaceDelimitador := Value;
  FBloco_B.ReplaceDelimitador := Value;
  FBloco_C.ReplaceDelimitador := Value;
  FBloco_D.ReplaceDelimitador := Value;
  FBloco_E.ReplaceDelimitador := Value;
  FBloco_G.ReplaceDelimitador := Value;
  FBloco_H.ReplaceDelimitador := Value;
  FBloco_K.ReplaceDelimitador := Value;
  FBloco_9.ReplaceDelimitador := Value;
end;

function TACBrSPEDFiscal.GetCurMascara: String;
begin
  Result := FCurMascara;
end;

procedure TACBrSPEDFiscal.SetCurMascara(const Value: String);
begin
  if Value = '' then
     raise EACBrSPEDFiscalException.Create('Campo não pode ser vazio! Para deixar sem mascara digite #');

	 FCurMascara := Value;

  FBloco_0.CurMascara := Value;
  FBloco_1.CurMascara := Value;
  FBloco_B.CurMascara := Value;
  FBloco_C.CurMascara := Value;
  FBloco_D.CurMascara := Value;
  FBloco_E.CurMascara := Value;
  FBloco_G.CurMascara := Value;
  FBloco_H.CurMascara := Value;
  FBloco_K.CurMascara := Value;
  FBloco_9.CurMascara := Value;
end;

function TACBrSPEDFiscal.GetTrimString: boolean;
begin
  Result := FTrimString;
end;

procedure TACBrSPEDFiscal.SetTrimString(const Value: boolean);
begin
  FTrimString := Value;

  FBloco_0.TrimString := Value;
  FBloco_1.TrimString := Value;
  FBloco_B.TrimString := Value;
  FBloco_C.TrimString := Value;
  FBloco_D.TrimString := Value;
  FBloco_E.TrimString := Value;
  FBloco_G.TrimString := Value;
  FBloco_H.TrimString := Value;
  FBloco_K.TrimString := Value;
  FBloco_9.TrimString := Value;
end;

function TACBrSPEDFiscal.GetDT_INI: TDateTime;
begin
  Result := FDT_INI;
end;

procedure TACBrSPEDFiscal.InicializaBloco( Bloco: TACBrSPED ) ;
begin
   Bloco.NomeArquivo  := FACBrTXT.NomeArquivo;
   Bloco.LinhasBuffer := FACBrTXT.LinhasBuffer;
   Bloco.Gravado      := False ;
   Bloco.Conteudo.Clear;
end;

procedure TACBrSPEDFiscal.IniciaGeracao;
begin
  if FInicializado then exit ;

  if FDT_INI = 0 then
    raise EACBrSPEDFiscalException.Create(ACBrStr('Informe a data inicial das informações contidas no arquivo!'));

  if FDT_FIN = 0 then
    raise EACBrSPEDFiscalException.Create(ACBrStr('Informe a data final das informações contidas no arquivo!'));

  if (Trim(FArquivo) = '') or (Trim(FPath) = '') then
    raise EACBrSPEDFiscalException.Create(ACBrStr('Caminho ou nome do arquivo não informado!'));

  FACBrTXT.NomeArquivo := FPath + FArquivo ;
  FACBrTXT.Reset;    // Apaga o Arquivo e limpa memória

  InicializaBloco( Bloco_0 ) ;
  InicializaBloco( Bloco_B ) ;
  InicializaBloco( Bloco_C ) ;
  InicializaBloco( Bloco_D ) ;
  InicializaBloco( Bloco_E ) ;
  InicializaBloco( Bloco_G ) ;
  InicializaBloco( Bloco_H ) ;
  InicializaBloco( Bloco_K ) ;
  InicializaBloco( Bloco_1 ) ;
  InicializaBloco( Bloco_9 ) ;


  FACBrTXT.Check(DayOf(FDT_INI) = 1, 'CHECAGEM INICIAL: A data inicial deve corresponder ao primeiro dia do mês informado!');
  FACBrTXT.Check(FDT_FIN >= FDT_INI, 'CHECAGEM INICIAL: A data final deve se maior que a data inicial!');
//  FACBrTXT.Check(FDT_FIN <= Date, 'CHECAGEM INICIAL: A data final "%s" não pode ser superior a data atual "%s"!',[DateToStr(FDT_FIN), DateToStr(Date)]);
  FACBrTXT.Check(DateOf(EndOfTheMonth(FDT_FIN)) = DateOf(FDT_FIN),'CHECAGEM INICIAL: A data final deve corresponder ao último dia do mês informado!');

  /// Preparação para totalizações de registros.
  Bloco_0.Registro0990.QTD_LIN_0 := 0;
  Bloco_1.Registro1990.QTD_LIN_1 := 0;
  Bloco_B.RegistroB990.QTD_LIN_B := 0;
  Bloco_C.RegistroC990.QTD_LIN_C := 0;
  Bloco_D.RegistroD990.QTD_LIN_D := 0;
  Bloco_E.RegistroE990.QTD_LIN_E := 0;
  Bloco_G.RegistroG990.QTD_LIN_G := 0;
  Bloco_K.RegistroK990.QTD_LIN_K := 0;
  Bloco_H.RegistroH990.QTD_LIN_H := 0;
  Bloco_9.Registro9990.QTD_LIN_9 := 0;
  Bloco_9.Registro9999.QTD_LIN   := 0;

  /// Limpa a lista
  Bloco_9.Registro9900.Clear;

  FInicializado := True ;
end;

procedure TACBrSPEDFiscal.SetArquivo(const Value: String);
var
  APath : String;
begin
  if FArquivo = Value then
     exit;

  FArquivo := ExtractFileName( Value );
  APath    := ExtractFilePath( Value );

  if APath <> '' then
     Path := APath;
end;

procedure TACBrSPEDFiscal.SetDT_INI(const Value: TDateTime);
begin
  FDT_INI := Value;

  FBloco_0.DT_INI := Value;
  FBloco_1.DT_INI := Value;
  FBloco_9.DT_INI := Value;
  FBloco_B.DT_INI := Value;
  FBloco_C.DT_INI := Value;
  FBloco_D.DT_INI := Value;
  FBloco_E.DT_INI := Value;
  FBloco_G.DT_INI := Value;
  FBloco_H.DT_INI := Value;
  FBloco_K.DT_INI := Value;

  if Assigned(FBloco_0) then
    FBloco_0.Registro0000.DT_INI := Value;
end;

function TACBrSPEDFiscal.GetDT_FIN: TDateTime;
begin
  Result := FDT_FIN;
end;

procedure TACBrSPEDFiscal.SetDT_FIN(const Value: TDateTime);
begin
  FDT_FIN := Value;

  FBloco_0.DT_FIN := Value;
  FBloco_1.DT_FIN := Value;
  FBloco_9.DT_FIN := Value;
  FBloco_B.DT_FIN := Value;
  FBloco_C.DT_FIN := Value;
  FBloco_D.DT_FIN := Value;
  FBloco_E.DT_FIN := Value;
  FBloco_G.DT_FIN := Value;
  FBloco_H.DT_FIN := Value;
  FBloco_K.DT_FIN := Value;

  if Assigned(FBloco_0) then
    FBloco_0.Registro0000.DT_FIN := Value;
end;

function TACBrSPEDFiscal.GetOnError: TErrorEvent;
begin
  Result := FOnError;
end;

function TACBrSPEDFiscal.GetReplaceDelimitador: Boolean;
begin
  Result := FReplaceDelimitador;
end;

procedure TACBrSPEDFiscal.SetOnError(const Value: TErrorEvent);
begin
  FOnError := Value;

  FBloco_0.OnError := Value;
  FBloco_1.OnError := Value;
  FBloco_B.OnError := Value;
  FBloco_C.OnError := Value;
  FBloco_D.OnError := Value;
  FBloco_E.OnError := Value;
  FBloco_G.OnError := Value;
  FBloco_H.OnError := Value;
  FBloco_K.OnError := Value;
  FBloco_9.OnError := Value;
end;

procedure TACBrSPEDFiscal.SaveFileTXT;
begin
  try
    IniciaGeracao;

    WriteBloco_0;
    WriteBloco_B;
    WriteBloco_C( True );    // True = Fecha o Bloco
    WriteBloco_D;
    WriteBloco_E;
    WriteBloco_G;
    WriteBloco_H;
    WriteBloco_K;
    WriteBloco_1;
    WriteBloco_9;
  finally
    /// Limpa de todos os Blocos as listas de todos os registros.
    LimpaRegistros;
    FACBrTXT.Conteudo.Clear;

    FInicializado := False ;
  end;
end;

procedure TACBrSPEDFiscal.WriteBloco_0;
begin
  if Bloco_0.Gravado then exit ;

  if not FInicializado then
     raise EACBrSPEDFiscalException.Create( 'Métodos "IniciaGeracao" não foi executado' );

  // BLOCO 0
  WriteRegistro0000;
  WriteRegistro0001;
  WriteRegistro0990;
  Bloco_0.WriteBuffer;
  Bloco_0.Conteudo.Clear;
  Bloco_0.Gravado := True;
end;


procedure TACBrSPEDFiscal.WriteBloco_B;
begin
   if Bloco_B.Gravado then
     exit ;

   if not Bloco_0.Gravado then
     WriteBloco_0;

   /// Alteração da minuta que terá validada a partir de 01 de janeiro de 2019
   if DT_INI >= EncodeDate(2019,01,01) then
   begin
     WriteRegistroB001;
     WriteRegistroB990;
     Bloco_B.WriteBuffer;
   end;
   Bloco_B.Conteudo.Clear;
   Bloco_B.Gravado := True ;
end;

procedure TACBrSPEDFiscal.WriteBloco_C( FechaBloco : Boolean );
begin
   if Bloco_C.Gravado then exit ;

   if not Bloco_B.Gravado then
      WriteBloco_B ;

   /// BLOCO C
   WriteRegistroC001;

   if Bloco_C.RegistroC001.IND_MOV = imSemDados then
      FechaBloco := True ;

   if FechaBloco then
      WriteRegistroC990;

   Bloco_C.WriteBuffer;
   Bloco_C.Conteudo.Clear;

   Bloco_C.Gravado := FechaBloco;
end;

procedure TACBrSPEDFiscal.WriteBloco_D;
begin
   if Bloco_D.Gravado then exit ;

   if not Bloco_C.Gravado then
      WriteBloco_C(True);

   /// BLOCO D
   WriteRegistroD001;
   WriteRegistroD990;
   Bloco_D.WriteBuffer;
   Bloco_D.Conteudo.Clear;
   Bloco_D.Gravado := True ;
end;

procedure TACBrSPEDFiscal.WriteBloco_E;
begin
   if Bloco_E.Gravado then exit ;

   if not Bloco_D.Gravado then
      WriteBloco_D;

   /// BLOCO E
   WriteRegistroE001;
   WriteRegistroE990;
   Bloco_E.WriteBuffer;
   Bloco_E.Conteudo.Clear;
   Bloco_E.Gravado := True ;
end;

procedure TACBrSPEDFiscal.WriteBloco_G;
begin
   if Bloco_G.Gravado then exit ;

   if not Bloco_E.Gravado then
      WriteBloco_E;

   /// Este ato entra em vigor na data de sua publicação, produzindo efeitos
   /// para as escriturações referentes aos períodos a partir de 1º de  janeiro de 2010,
   /// --> exceto quanto ao BLOCO G e registros pertinentes ao Livro de
   /// Controle de Crédito de ICMS do Ativo Permanente cujos efeitos serão
   /// a partir de 1º de julho de 2010 <--.
   /// Exigência do Art. 3º do AC 09/08
   ///
   /// Prorrogado para 01/01/2011 conforme Guia Prático da EFD 2.01
   /// *Bloco G incluído para vigorar a partir do período de apuração de janeiro de 2011.
   if DT_INI >= EncodeDate(2011,01,01) then
   begin
     /// BLOCO G
     WriteRegistroG001;
     WriteRegistroG990;
     Bloco_G.WriteBuffer;
   end;

   Bloco_G.Conteudo.Clear;
   Bloco_G.Gravado := True ;
end;

procedure TACBrSPEDFiscal.WriteBloco_H;
begin
   if Bloco_H.Gravado then exit ;

   if not Bloco_G.Gravado then
      WriteBloco_G;

   /// BLOCO H
   WriteRegistroH001;
   WriteRegistroH990;
   Bloco_H.WriteBuffer;
   Bloco_H.Conteudo.Clear;
   Bloco_H.Gravado := True ;
end;

procedure TACBrSPEDFiscal.WriteBloco_K;
begin
   if Bloco_K.Gravado then exit ;

   if not Bloco_H.Gravado then
      WriteBloco_H;

   /// Alteração da minuta que terá validada a partir de 01 de janeiro de 2015
	 /// mudou e será a partir de 01 janeiro 2016
   if DT_INI >= EncodeDate(2016,01,01) then
   begin
     /// BLOCO K
     WriteRegistroK001;
     WriteRegistroK990;
     Bloco_K.WriteBuffer;
   end;
   Bloco_K.Conteudo.Clear;
   Bloco_K.Gravado := True ;
end;

procedure TACBrSPEDFiscal.WriteBloco_1;
begin
   if Bloco_1.Gravado then exit ;

   if not Bloco_K.Gravado then
      WriteBloco_K;

   /// BLOCO 1
   WriteRegistro1001;
   WriteRegistro1990;
   Bloco_1.WriteBuffer;
   Bloco_1.Conteudo.Clear;
   Bloco_1.Gravado := True ;
end;

procedure TACBrSPEDFiscal.WriteBloco_9;
begin
   if Bloco_9.Gravado then exit ;

   if not Bloco_1.Gravado then
      WriteBloco_1 ;

   /// BLOCO 9
   WriteRegistro9001;
   WriteRegistro9900;
   WriteRegistro9990;
   WriteRegistro9999;
   Bloco_9.WriteBuffer;
   Bloco_9.Conteudo.Clear;
   Bloco_9.Gravado := True ;
end;

procedure TACBrSPEDFiscal.WriteRegistro0000;
begin
   with Bloco_9.Registro9900.New do
   begin
      REG_BLC := '0000';
      QTD_REG_BLC := 1;
   end;
   Bloco_0.WriteRegistro0000;
end;

procedure TACBrSPEDFiscal.WriteRegistro0001;
begin
   // Preenche as classes com os dados
   Bloco_0.WriteRegistro0001;
   //
   with Bloco_9.Registro9900 do
   begin
      with New do
      begin
         REG_BLC := '0001';
         QTD_REG_BLC := 1;
      end;
   end;
   if (DT_INI >= EncodeDate(2020,01,01)) and 
      (Bloco_0.Registro0000.IND_ATIV = atIndustrial) then
   begin 
     with Bloco_9.Registro9900 do
     begin
       with New do
       begin
         REG_BLC := '0002';
         QTD_REG_BLC := 1;
       end;
     end;
   end;
   if Bloco_0.Registro0001.IND_MOV = imComDados then
   begin
      with Bloco_9.Registro9900 do
      begin
         if Bloco_0.Registro0005Count > 0 then
         begin
            with New do
            begin
               REG_BLC := '0005';
               QTD_REG_BLC := Bloco_0.Registro0005Count;
            end;
         end;
         if Bloco_0.Registro0015Count > 0 then
         begin
            with New do
            begin
               REG_BLC := '0015';
               QTD_REG_BLC := Bloco_0.Registro0015Count;
            end;
         end;
         with New do
         begin
            REG_BLC := '0100';
            QTD_REG_BLC := 1;
         end;
         if Bloco_0.Registro0150Count > 0 then
         begin
            with New do
            begin
               REG_BLC := '0150';
               QTD_REG_BLC := Bloco_0.Registro0150Count;
            end;
         end;
         if Bloco_0.Registro0175Count > 0 then
         begin
            with New do
            begin
               REG_BLC := '0175';
               QTD_REG_BLC := Bloco_0.Registro0175Count;
            end;
         end;
         if Bloco_0.Registro0190Count > 0 then
         begin
            with New do
            begin
               REG_BLC := '0190';
               QTD_REG_BLC := Bloco_0.Registro0190Count;
            end;
         end;
         if Bloco_0.Registro0200Count > 0 then
         begin
            with New do
            begin
               REG_BLC := '0200';
               QTD_REG_BLC := Bloco_0.Registro0200Count;
            end;
         end;
         if Bloco_0.Registro0205Count > 0 then
         begin
            with New do
            begin
               REG_BLC := '0205';
               QTD_REG_BLC := Bloco_0.Registro0205Count;
            end;
         end;
         if Bloco_0.Registro0206Count > 0 then
         begin
            with New do
            begin
               REG_BLC := '0206';
               QTD_REG_BLC := Bloco_0.Registro0206Count;
            end;
         end;

         // Jorge 20/08/14
         if Bloco_0.Registro0210Count > 0 then
         begin
            with New do
            begin
               REG_BLC := '0210';
               QTD_REG_BLC := Bloco_0.Registro0210Count;
            end;
         end;

         if Bloco_0.Registro0220Count > 0 then
         begin
            with New do
            begin
               REG_BLC := '0220';
               QTD_REG_BLC := Bloco_0.Registro0220Count;
            end;
         end;

         if (Bloco_0.Registro0221Count > 0) then
         begin
            with New do
            begin
               REG_BLC := '0221';
               QTD_REG_BLC := Bloco_0.Registro0221Count;
            end;
         end;

         /// Exigência do Art. 3º do AC 09/08
         ///
         /// Prorrogado para 01/01/2011 conforme Guia Prático da EFD 2.01
         /// *Bloco G incluído para vigorar a partir do período de apuração de janeiro de 2011.
         if DT_INI >= EncodeDate(2011,01,01) then
         begin
            if Bloco_0.Registro0300Count > 0 then
            begin
               with New do
               begin
                  REG_BLC := '0300';
                  QTD_REG_BLC := Bloco_0.Registro0300Count;
               end;
            end;
            if Bloco_0.Registro0305Count > 0 then
            begin
               with New do
               begin
                  REG_BLC := '0305';
                  QTD_REG_BLC := Bloco_0.Registro0305Count;
               end;
            end;
         end;
         if Bloco_0.Registro0400Count > 0 then
         begin
            with New do
            begin
               REG_BLC := '0400';
               QTD_REG_BLC := Bloco_0.Registro0400Count;
            end;
         end;
         if Bloco_0.Registro0450Count > 0 then
         begin
            with New do
            begin
               REG_BLC := '0450';
               QTD_REG_BLC := Bloco_0.Registro0450Count;
            end;
         end;
         if Bloco_0.Registro0460Count > 0 then
         begin
            with New do
            begin
               REG_BLC := '0460';
               QTD_REG_BLC := Bloco_0.Registro0460Count;
            end;
         end;
         /// Exigência do Art. 3º do AC 09/08
         ///
         /// Prorrogado para 01/01/2011 conforme Guia Prático da EFD 2.01
         /// *Bloco G incluído para vigorar a partir do período de apuração de janeiro de 2011.
         if DT_INI >= EncodeDate(2011,01,01) then
         begin
            if Bloco_0.Registro0500Count > 0 then
            begin
               with New do
               begin
                  REG_BLC := '0500';
                  QTD_REG_BLC := Bloco_0.Registro0500Count;
               end;
            end;
            if Bloco_0.Registro0600Count > 0 then
            begin
               with New do
               begin
                  REG_BLC := '0600';
                  QTD_REG_BLC := Bloco_0.Registro0600Count;
               end;
            end;
         end;
      end;
   end;
end;

procedure TACBrSPEDFiscal.WriteRegistro0990;
begin
   with Bloco_9.Registro9900.New do
   begin
      REG_BLC := '0990';
      QTD_REG_BLC := 1;
   end;
   Bloco_0.WriteRegistro0990;
end;

procedure TACBrSPEDFiscal.WriteRegistro1001;
begin
   Bloco_1.WriteRegistro1001;
   //
   with Bloco_9.Registro9900 do
   begin
      with New do
      begin
         REG_BLC := '1001';
         QTD_REG_BLC := 1;
      end;
   end;
   if Bloco_1.Registro1001.IND_MOV = imComDados then
   begin
      with Bloco_9.Registro9900 do
      begin
         if Bloco_1.Registro1010Count > 0 then
         begin
            with New do
            begin
               REG_BLC := '1010';
               QTD_REG_BLC := Bloco_1.Registro1010Count;
            end;
         end;
         if Bloco_1.Registro1100Count > 0 then
         begin
            with New do
            begin
               REG_BLC := '1100';
               QTD_REG_BLC := Bloco_1.Registro1100Count;
            end;
         end;
         if Bloco_1.Registro1105Count > 0 then
         begin
            with New do
            begin
               REG_BLC := '1105';
               QTD_REG_BLC := Bloco_1.Registro1105Count;
            end;
         end;
         if Bloco_1.Registro1110Count > 0 then
         begin
            with New do
            begin
               REG_BLC := '1110';
               QTD_REG_BLC := Bloco_1.Registro1110Count;
            end;
         end;
         if Bloco_1.Registro1200Count > 0 then
         begin
            with New do
            begin
               REG_BLC := '1200';
               QTD_REG_BLC := Bloco_1.Registro1200Count;
            end;
         end;
         if Bloco_1.Registro1210Count > 0 then
         begin
            with New do
            begin
               REG_BLC := '1210';
               QTD_REG_BLC := Bloco_1.Registro1210Count;
            end;
         end;
		 if Bloco_1.Registro1250Count > 0 then
         begin
            with New do
            begin
               REG_BLC := '1250';
               QTD_REG_BLC := Bloco_1.Registro1250Count;
            end;
         end;
		 if Bloco_1.Registro1255Count > 0 then
         begin
            with New do
            begin
               REG_BLC := '1255';
               QTD_REG_BLC := Bloco_1.Registro1255Count;
            end;
         end;
         if Bloco_1.Registro1300Count > 0 then
         begin
            with New do
            begin
               REG_BLC := '1300';
               QTD_REG_BLC := Bloco_1.Registro1300Count;
            end;
         end;
         if Bloco_1.Registro1310Count > 0 then
         begin
            with New do
            begin
               REG_BLC := '1310';
               QTD_REG_BLC := Bloco_1.Registro1310Count;
            end;
         end;
         if Bloco_1.Registro1320Count > 0 then
         begin
            with New do
            begin
               REG_BLC := '1320';
               QTD_REG_BLC := Bloco_1.Registro1320Count;
            end;
         end;
         if Bloco_1.Registro1350Count > 0 then
         begin
            with New do
            begin
               REG_BLC := '1350';
               QTD_REG_BLC := Bloco_1.Registro1350Count;
            end;
         end;
         if Bloco_1.Registro1360Count > 0 then
         begin
            with New do
            begin
               REG_BLC := '1360';
               QTD_REG_BLC := Bloco_1.Registro1360Count;
            end;
         end;
         if Bloco_1.Registro1370Count > 0 then
         begin
            with New do
            begin
               REG_BLC := '1370';
               QTD_REG_BLC := Bloco_1.Registro1370Count;
            end;
         end;
         if Bloco_1.Registro1390Count > 0 then
         begin
            with New do
            begin
               REG_BLC := '1390';
               QTD_REG_BLC := Bloco_1.Registro1390Count;
            end;
         end;
         if Bloco_1.Registro1391Count > 0 then
         begin
            with New do
            begin
               REG_BLC := '1391';
               QTD_REG_BLC := Bloco_1.Registro1391Count;
            end;
         end;
         if Bloco_1.Registro1400Count > 0 then
         begin
            with New do
            begin
               REG_BLC := '1400';
               QTD_REG_BLC := Bloco_1.Registro1400Count;
            end;
         end;
         if Bloco_1.Registro1500Count > 0 then
         begin
            with New do
            begin
               REG_BLC := '1500';
               QTD_REG_BLC := Bloco_1.Registro1500Count;
            end;
         end;
         if Bloco_1.Registro1510Count > 0 then
         begin
            with New do
            begin
               REG_BLC := '1510';
               QTD_REG_BLC := Bloco_1.Registro1510Count;
            end;
         end;
         if Bloco_1.Registro1600Count > 0 then
         begin
            with New do
            begin
               REG_BLC := '1600';
               QTD_REG_BLC := Bloco_1.Registro1600Count;
            end;
         end;
         if Bloco_1.Registro1601Count > 0 then
         begin
            with New do
            begin
               REG_BLC := '1601';
               QTD_REG_BLC := Bloco_1.Registro1601Count;
            end;
         end;
         if Bloco_1.Registro1700Count > 0 then
         begin
            with New do
            begin
               REG_BLC := '1700';
               QTD_REG_BLC := Bloco_1.Registro1700Count;
            end;
         end;
         if Bloco_1.Registro1710Count > 0 then
         begin
            with New do
            begin
               REG_BLC := '1710';
               QTD_REG_BLC := Bloco_1.Registro1710Count;
            end;
         end;
         if Bloco_1.Registro1800Count > 0 then
         begin
            with New do
            begin
               REG_BLC := '1800';
               QTD_REG_BLC := Bloco_1.Registro1800Count;
            end;
         end;
         if Bloco_1.Registro1900Count > 0 then
         begin
            with New do
            begin
               REG_BLC := '1900';
               QTD_REG_BLC := Bloco_1.Registro1900Count;
            end;
         end;
         if Bloco_1.Registro1910Count > 0 then
         begin
            with New do
            begin
               REG_BLC := '1910';
               QTD_REG_BLC := Bloco_1.Registro1910Count;
            end;
         end;
         if Bloco_1.Registro1920Count > 0 then
         begin
            with New do
            begin
               REG_BLC := '1920';
               QTD_REG_BLC := Bloco_1.Registro1920Count;
            end;
         end;
         if Bloco_1.Registro1921Count > 0 then
         begin
            with New do
            begin
               REG_BLC := '1921';
               QTD_REG_BLC := Bloco_1.Registro1921Count;
            end;
         end;
         if Bloco_1.Registro1922Count > 0 then
         begin
            with New do
            begin
               REG_BLC := '1922';
               QTD_REG_BLC := Bloco_1.Registro1922Count;
            end;
         end;
         if Bloco_1.Registro1923Count > 0 then
         begin
            with New do
            begin
               REG_BLC := '1923';
               QTD_REG_BLC := Bloco_1.Registro1923Count;
            end;
         end;
         if Bloco_1.Registro1925Count > 0 then
         begin
            with New do
            begin
               REG_BLC := '1925';
               QTD_REG_BLC := Bloco_1.Registro1925Count;
            end;
         end;
         if Bloco_1.Registro1926Count > 0 then
         begin
            with New do
            begin
               REG_BLC := '1926';
               QTD_REG_BLC := Bloco_1.Registro1926Count;
            end;
         end;
         if Bloco_1.Registro1960Count > 0 then
         begin
            with New do
            begin
               REG_BLC := '1960';
               QTD_REG_BLC := Bloco_1.Registro1960Count;
            end;
         end;
         if Bloco_1.Registro1970Count > 0 then
         begin
            with New do
            begin
               REG_BLC := '1970';
               QTD_REG_BLC := Bloco_1.Registro1970Count;
            end;
         end;
         if Bloco_1.Registro1975Count > 0 then
         begin
            with New do
            begin
               REG_BLC := '1975';
               QTD_REG_BLC := Bloco_1.Registro1975Count;
            end;
         end;
         if Bloco_1.Registro1980Count > 0 then
         begin
            with New do
            begin
               REG_BLC := '1980';
               QTD_REG_BLC := Bloco_1.Registro1980Count;
            end;
         end;
      end;
   end;
end;

procedure TACBrSPEDFiscal.WriteRegistro1990;
begin
   with Bloco_9.Registro9900.New do
   begin
      REG_BLC := '1990';
      QTD_REG_BLC := 1;
   end;
   Bloco_1.WriteRegistro1990;
end;

procedure TACBrSPEDFiscal.WriteRegistroC001;
begin
  Bloco_C.WriteRegistroC001;
end;

procedure TACBrSPEDFiscal.WriteRegistroC990;
begin
  with Bloco_9.Registro9900 do
  begin
     with New do
     begin
        REG_BLC := 'C001';
        QTD_REG_BLC := 1;
     end;
  end;

  if Bloco_C.RegistroC001.IND_MOV = imComDados then
  begin
    with Bloco_9.Registro9900 do
    begin
       if Bloco_C.RegistroC100Count > 0 then
       begin
         with New do
         begin
           REG_BLC := 'C100';
           QTD_REG_BLC := Bloco_C.RegistroC100Count;
         end;
       end;
       if Bloco_C.RegistroC101Count > 0 then
       begin
         with New do
         begin
           REG_BLC := 'C101';
           QTD_REG_BLC := Bloco_C.RegistroC101Count;
         end;
       end;
       if Bloco_C.RegistroC105Count > 0 then
       begin
         with New do
         begin
           REG_BLC := 'C105';
           QTD_REG_BLC := Bloco_C.RegistroC105Count;
         end;
       end;
       if Bloco_C.RegistroC110Count > 0 then
       begin
         with New do
         begin
           REG_BLC := 'C110';
           QTD_REG_BLC := Bloco_C.RegistroC110Count;
         end;
       end;
       if Bloco_C.RegistroC111Count > 0 then
       begin
         with New do
         begin
           REG_BLC := 'C111';
           QTD_REG_BLC := Bloco_C.RegistroC111Count;
         end;
       end;
       if Bloco_C.RegistroC112Count > 0 then
       begin
         with New do
         begin
           REG_BLC := 'C112';
           QTD_REG_BLC := Bloco_C.RegistroC112Count;
         end;
       end;
       if Bloco_C.RegistroC113Count > 0 then
       begin
         with New do
         begin
           REG_BLC := 'C113';
           QTD_REG_BLC := Bloco_C.RegistroC113Count;
         end;
       end;
       if Bloco_C.RegistroC114Count > 0 then
       begin
         with New do
         begin
           REG_BLC := 'C114';
           QTD_REG_BLC := Bloco_C.RegistroC114Count;
         end;
       end;
       if Bloco_C.RegistroC115Count > 0 then
       begin
         with New do
         begin
           REG_BLC := 'C115';
           QTD_REG_BLC := Bloco_C.RegistroC115Count;
         end;
       end;
       if Bloco_C.RegistroC116Count > 0 then
       begin
         with New do
         begin
           REG_BLC := 'C116';
           QTD_REG_BLC := Bloco_C.RegistroC116Count;
         end;
       end;
       if Bloco_C.RegistroC120Count > 0 then
       begin
         with New do
         begin
           REG_BLC := 'C120';
           QTD_REG_BLC := Bloco_C.RegistroC120Count;
         end;
       end;
       if Bloco_C.RegistroC130Count > 0 then
       begin
         with New do
         begin
           REG_BLC := 'C130';
           QTD_REG_BLC := Bloco_C.RegistroC130Count;
         end;
       end;
       if Bloco_C.RegistroC140Count > 0 then
       begin
         with New do
         begin
           REG_BLC := 'C140';
           QTD_REG_BLC := Bloco_C.RegistroC140Count;
         end;
       end;
       if Bloco_C.RegistroC141Count > 0 then
       begin
         with New do
         begin
           REG_BLC := 'C141';
           QTD_REG_BLC := Bloco_C.RegistroC141Count;
         end;
       end;
       if Bloco_C.RegistroC160Count > 0 then
       begin
         with New do
         begin
           REG_BLC := 'C160';
           QTD_REG_BLC := Bloco_C.RegistroC160Count;
         end;
       end;
       if Bloco_C.RegistroC165Count > 0 then
       begin
         with New do
         begin
           REG_BLC := 'C165';
           QTD_REG_BLC := Bloco_C.RegistroC165Count;
         end;
       end;
       if Bloco_C.RegistroC170Count > 0 then
       begin
         with New do
         begin
           REG_BLC := 'C170';
           QTD_REG_BLC := Bloco_C.RegistroC170Count;
         end;
       end;
       if Bloco_C.RegistroC171Count > 0 then
       begin
         with New do
         begin
           REG_BLC := 'C171';
           QTD_REG_BLC := Bloco_C.RegistroC171Count;
         end;
       end;
       if Bloco_C.RegistroC172Count > 0 then
       begin
         with New do
         begin
           REG_BLC := 'C172';
           QTD_REG_BLC := Bloco_C.RegistroC172Count;
         end;
       end;
       if Bloco_C.RegistroC173Count > 0 then
       begin
         with New do
         begin
           REG_BLC := 'C173';
           QTD_REG_BLC := Bloco_C.RegistroC173Count;
         end;
       end;
       if Bloco_C.RegistroC174Count > 0 then
       begin
         with New do
         begin
           REG_BLC := 'C174';
           QTD_REG_BLC := Bloco_C.RegistroC174Count;
         end;
       end;
       if Bloco_C.RegistroC175Count > 0 then
       begin
         with New do
         begin
           REG_BLC := 'C175';
           QTD_REG_BLC := Bloco_C.RegistroC175Count;
         end;
       end;
       if Bloco_C.RegistroC176Count > 0 then
       begin
         with New do
         begin
           REG_BLC := 'C176';
           QTD_REG_BLC := Bloco_C.RegistroC176Count;
         end;
       end;
       if Bloco_C.RegistroC177Count > 0 then
       begin
         with New do
         begin
           REG_BLC := 'C177';
           QTD_REG_BLC := Bloco_C.RegistroC177Count;
         end;
       end;
       if Bloco_C.RegistroC178Count > 0 then
       begin
         with New do
         begin
           REG_BLC := 'C178';
           QTD_REG_BLC := Bloco_C.RegistroC178Count;
         end;
       end;
       if Bloco_C.RegistroC179Count > 0 then
       begin
         with New do
         begin
           REG_BLC := 'C179';
           QTD_REG_BLC := Bloco_C.RegistroC179Count;
         end;
       end;
       if Bloco_C.RegistroC180Count > 0 then
       begin
         with New do
         begin
           REG_BLC := 'C180';
           QTD_REG_BLC := Bloco_C.RegistroC180Count;
         end;
       end;
       if Bloco_C.RegistroC181Count > 0 then
       begin
         with New do
         begin
           REG_BLC := 'C181';
           QTD_REG_BLC := Bloco_C.RegistroC181Count;
         end;
       end;
       if Bloco_C.RegistroC185Count > 0 then
       begin
         with New do
         begin
           REG_BLC := 'C185';
           QTD_REG_BLC := Bloco_C.RegistroC185Count;
         end;
       end;
       if Bloco_C.RegistroC186Count > 0 then
       begin
         with New do
         begin
           REG_BLC := 'C186';
           QTD_REG_BLC := Bloco_C.RegistroC186Count;
         end;
       end;
       if Bloco_C.RegistroC190Count > 0 then
       begin
         with New do
         begin
           REG_BLC := 'C190';
           QTD_REG_BLC := Bloco_C.RegistroC190Count;
         end;
       end;
       if Bloco_C.RegistroC191Count > 0 then
       begin
         with New do
         begin
           REG_BLC := 'C191';
           QTD_REG_BLC := Bloco_C.RegistroC191Count;
         end;
       end;
       if Bloco_C.RegistroC195Count > 0 then
       begin
         with New do
         begin
           REG_BLC := 'C195';
           QTD_REG_BLC := Bloco_C.RegistroC195Count;
         end;
       end;
       if Bloco_C.RegistroC197Count > 0 then
       begin
         with New do
         begin
           REG_BLC := 'C197';
           QTD_REG_BLC := Bloco_C.RegistroC197Count;
         end;
       end;
       if Bloco_C.RegistroC300Count > 0 then
       begin
         with New do
         begin
           REG_BLC := 'C300';
           QTD_REG_BLC := Bloco_C.RegistroC300Count;
         end;
       end;
       if Bloco_C.RegistroC310Count > 0 then
       begin
         with New do
         begin
           REG_BLC := 'C310';
           QTD_REG_BLC := Bloco_C.RegistroC310Count;
         end;
       end;
       if Bloco_C.RegistroC320Count > 0 then
       begin
         with New do
         begin
           REG_BLC := 'C320';
           QTD_REG_BLC := Bloco_C.RegistroC320Count;
         end;
       end;
       if Bloco_C.RegistroC321Count > 0 then
       begin
         with New do
         begin
           REG_BLC := 'C321';
           QTD_REG_BLC := Bloco_C.RegistroC321Count;
         end;
       end;
       if Bloco_C.RegistroC350Count > 0 then
       begin
         with New do
         begin
           REG_BLC := 'C350';
           QTD_REG_BLC := Bloco_C.RegistroC350Count;
         end;
       end;
       if Bloco_C.RegistroC370Count > 0 then
       begin
         with New do
         begin
           REG_BLC := 'C370';
           QTD_REG_BLC := Bloco_C.RegistroC370Count;
         end;
       end;
       if Bloco_C.RegistroC390Count > 0 then
       begin
         with New do
         begin
           REG_BLC := 'C390';
           QTD_REG_BLC := Bloco_C.RegistroC390Count;
         end;
       end;
       if Bloco_C.RegistroC400Count > 0 then
       begin
         with New do
         begin
           REG_BLC := 'C400';
           QTD_REG_BLC := Bloco_C.RegistroC400Count;
         end;
       end;
       if Bloco_C.RegistroC405Count > 0 then
       begin
         with New do
         begin
           REG_BLC := 'C405';
           QTD_REG_BLC := Bloco_C.RegistroC405Count;
         end;
       end;
       if Bloco_C.RegistroC410Count > 0 then
       begin
         with New do
         begin
           REG_BLC := 'C410';
           QTD_REG_BLC := Bloco_C.RegistroC410Count;
         end;
       end;
       if Bloco_C.RegistroC420Count > 0 then
       begin
         with New do
         begin
           REG_BLC := 'C420';
           QTD_REG_BLC := Bloco_C.RegistroC420Count;
         end;
       end;
       if Bloco_C.RegistroC425Count > 0 then
       begin
         with New do
         begin
           REG_BLC := 'C425';
           QTD_REG_BLC := Bloco_C.RegistroC425Count;
         end;
       end;
       if Bloco_C.RegistroC460Count > 0 then
       begin
         with New do
         begin
           REG_BLC := 'C460';
           QTD_REG_BLC := Bloco_C.RegistroC460Count;
         end;
       end;
       if Bloco_C.RegistroC465Count > 0 then
       begin
         with New do
         begin
           REG_BLC := 'C465';
           QTD_REG_BLC := Bloco_C.RegistroC465Count;
         end;
       end;
       if Bloco_C.RegistroC470Count > 0 then
       begin
         with New do
         begin
           REG_BLC := 'C470';
           QTD_REG_BLC := Bloco_C.RegistroC470Count;
         end;
       end;
       if Bloco_C.RegistroC490Count > 0 then
       begin
         with New do
         begin
           REG_BLC := 'C490';
           QTD_REG_BLC := Bloco_C.RegistroC490Count;
         end;
       end;
       if Bloco_C.RegistroC495Count > 0 then
       begin
         with New do
         begin
           REG_BLC := 'C495';
           QTD_REG_BLC := Bloco_C.RegistroC495Count;
         end;
       end;
       if Bloco_C.RegistroC500Count > 0 then
       begin
         with New do
         begin
           REG_BLC := 'C500';
           QTD_REG_BLC := Bloco_C.RegistroC500Count;
         end;
       end;
       if Bloco_C.RegistroC510Count > 0 then
       begin
         with New do
         begin
           REG_BLC := 'C510';
           QTD_REG_BLC := Bloco_C.RegistroC510Count;
         end;
       end;
       if Bloco_C.RegistroC590Count > 0 then
       begin
         with New do
         begin
           REG_BLC := 'C590';
           QTD_REG_BLC := Bloco_C.RegistroC590Count;
         end;
       end;
       if Bloco_C.RegistroC591Count > 0 then
       begin
         with New do
         begin
           REG_BLC := 'C591';
           QTD_REG_BLC := Bloco_C.RegistroC591Count;
         end;
       end;
       if Bloco_C.RegistroC595Count > 0 then
       begin
         with New do
         begin
           REG_BLC := 'C595';
           QTD_REG_BLC := Bloco_C.RegistroC595Count;
         end;
       end;
       if Bloco_C.RegistroC597Count > 0 then
       begin
         with New do
         begin
           REG_BLC := 'C597';
           QTD_REG_BLC := Bloco_C.RegistroC597Count;
         end;
       end;
       if Bloco_C.RegistroC600Count > 0 then
       begin
         with New do
         begin
           REG_BLC := 'C600';
           QTD_REG_BLC := Bloco_C.RegistroC600Count;
         end;
       end;
       if Bloco_C.RegistroC601Count > 0 then
       begin
         with New do
         begin
           REG_BLC := 'C601';
           QTD_REG_BLC := Bloco_C.RegistroC601Count;
         end;
       end;
       if Bloco_C.RegistroC610Count > 0 then
       begin
         with New do
         begin
           REG_BLC := 'C610';
           QTD_REG_BLC := Bloco_C.RegistroC610Count;
         end;
       end;
       if Bloco_C.RegistroC690Count > 0 then
       begin
         with New do
         begin
           REG_BLC := 'C690';
           QTD_REG_BLC := Bloco_C.RegistroC690Count;
         end;
       end;
       if Bloco_C.RegistroC700Count > 0 then
       begin
         with New do
         begin
           REG_BLC := 'C700';
           QTD_REG_BLC := Bloco_C.RegistroC700Count;
         end;
       end;
       if Bloco_C.RegistroC790Count > 0 then
       begin
         with New do
         begin
           REG_BLC := 'C790';
           QTD_REG_BLC := Bloco_C.RegistroC790Count;
         end;
       end;
       if Bloco_C.RegistroC791Count > 0 then
       begin
         with New do
         begin
           REG_BLC := 'C791';
           QTD_REG_BLC := Bloco_C.RegistroC791Count;
         end;
       end;

       if Bloco_C.RegistroC800Count > 0 then
       begin
         with New do
         begin
           REG_BLC := 'C800';
           QTD_REG_BLC := Bloco_C.RegistroC800Count;
         end;
       end;

       if Bloco_C.RegistroC810Count > 0 then
       begin
         with New do
         begin
           REG_BLC := 'C810';
           QTD_REG_BLC := Bloco_C.RegistroC810Count;
         end;
         with New do
         begin
           REG_BLC := 'C815';
           QTD_REG_BLC := Bloco_C.RegistroC810Count;
         end;
       end;

       if Bloco_C.RegistroC850Count > 0 then
       begin
         with New do
         begin
           REG_BLC := 'C850';
           QTD_REG_BLC := Bloco_C.RegistroC850Count;
         end;
       end;

       if Bloco_C.RegistroC855Count > 0 then
       begin
         with New do
         begin
           REG_BLC := 'C855';
           QTD_REG_BLC := Bloco_C.RegistroC855Count;
         end;
       end;

       if Bloco_C.RegistroC857Count > 0 then
       begin
         with New do
         begin
           REG_BLC := 'C857';
           QTD_REG_BLC := Bloco_C.RegistroC857Count;
         end;
       end;

       if Bloco_C.RegistroC860Count > 0 then
       begin
         with New do
         begin
           REG_BLC := 'C860';
           QTD_REG_BLC := Bloco_C.RegistroC860Count;
         end;
       end;

       if Bloco_C.RegistroC870Count > 0 then
       begin
         with New do
         begin
           REG_BLC := 'C870';
           QTD_REG_BLC := Bloco_C.RegistroC870Count;
         end;
         with New do
         begin
           REG_BLC := 'C880';
           QTD_REG_BLC := Bloco_C.RegistroC870Count;
         end;
       end;

       if Bloco_C.RegistroC890Count > 0 then
       begin
         with New do
         begin
           REG_BLC := 'C890';
           QTD_REG_BLC := Bloco_C.RegistroC890Count;
         end;
       end;

       if Bloco_C.RegistroC895Count > 0 then
       begin
         with New do
         begin
           REG_BLC := 'C895';
           QTD_REG_BLC := Bloco_C.RegistroC895Count;
         end;
       end;

       if Bloco_C.RegistroC897Count > 0 then
       begin
         with New do
         begin
           REG_BLC := 'C897';
           QTD_REG_BLC := Bloco_C.RegistroC897Count;
         end;
       end;

    end;
  end;

  with Bloco_9.Registro9900 do
  begin
     with New do
     begin
       REG_BLC := 'C990';
       QTD_REG_BLC := 1;
     end;
  end;

  Bloco_C.WriteRegistroC990;
end;

procedure TACBrSPEDFiscal.WriteRegistroD001;
begin
   Bloco_D.WriteRegistroD001;
   //
   with Bloco_9.Registro9900 do
   begin
      with New do
      begin
         REG_BLC := 'D001';
         QTD_REG_BLC := 1;
      end;
   end;
   if Bloco_D.RegistroD001.IND_MOV = imComDados then
   begin
      with Bloco_9.Registro9900 do
      begin
         if Bloco_D.RegistroD100Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'D100';
               QTD_REG_BLC := Bloco_D.RegistroD100Count;
            end;
         end;
				 if Bloco_D.RegistroD101Count > 0 then
				 begin
					 with New do
					 begin
						 REG_BLC := 'D101';
						 QTD_REG_BLC := Bloco_D.RegistroD101Count;
					 end;
				 end;
         if Bloco_D.RegistroD110Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'D110';
               QTD_REG_BLC := Bloco_D.RegistroD110Count;
            end;
         end;
         if Bloco_D.RegistroD120Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'D120';
               QTD_REG_BLC := Bloco_D.RegistroD120Count;
            end;
         end;
         if Bloco_D.RegistroD130Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'D130';
               QTD_REG_BLC := Bloco_D.RegistroD130Count;
            end;
         end;
         if Bloco_D.RegistroD140Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'D140';
               QTD_REG_BLC := Bloco_D.RegistroD140Count;
            end;
         end;
         if Bloco_D.RegistroD150Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'D150';
               QTD_REG_BLC := Bloco_D.RegistroD150Count;
            end;
         end;
         if Bloco_D.RegistroD160Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'D160';
               QTD_REG_BLC := Bloco_D.RegistroD160Count;
            end;
         end;
         if Bloco_D.RegistroD161Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'D161';
               QTD_REG_BLC := Bloco_D.RegistroD161Count;
            end;
         end;
         if Bloco_D.RegistroD162Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'D162';
               QTD_REG_BLC := Bloco_D.RegistroD162Count;
            end;
         end;
         if Bloco_D.RegistroD170Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'D170';
               QTD_REG_BLC := Bloco_D.RegistroD170Count;
            end;
         end;
         if Bloco_D.RegistroD180Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'D180';
               QTD_REG_BLC := Bloco_D.RegistroD180Count;
            end;
         end;
         if Bloco_D.RegistroD190Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'D190';
               QTD_REG_BLC := Bloco_D.RegistroD190Count;
            end;
         end;
         if Bloco_D.RegistroD195Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'D195';
               QTD_REG_BLC := Bloco_D.RegistroD195Count;
            end;
         end;
         if Bloco_D.RegistroD197Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'D197';
               QTD_REG_BLC := Bloco_D.RegistroD197Count;
            end;
         end;
		 
         if Bloco_D.RegistroD300Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'D300';
               QTD_REG_BLC := Bloco_D.RegistroD300Count;
            end;
         end;
         if Bloco_D.RegistroD301Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'D301';
               QTD_REG_BLC := Bloco_D.RegistroD301Count;
            end;
         end;
         if Bloco_D.RegistroD310Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'D310';
               QTD_REG_BLC := Bloco_D.RegistroD310Count;
            end;
         end;
         if Bloco_D.RegistroD350Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'D350';
               QTD_REG_BLC := Bloco_D.RegistroD350Count;
            end;
         end;
         if Bloco_D.RegistroD355Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'D355';
               QTD_REG_BLC := Bloco_D.RegistroD355Count;
            end;
         end;
         if Bloco_D.RegistroD360Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'D360';
               QTD_REG_BLC := Bloco_D.RegistroD360Count;
            end;
         end;
         if Bloco_D.RegistroD365Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'D365';
               QTD_REG_BLC := Bloco_D.RegistroD365Count;
            end;
         end;
         if Bloco_D.RegistroD370Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'D370';
               QTD_REG_BLC := Bloco_D.RegistroD370Count;
            end;
         end;
         if Bloco_D.RegistroD390Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'D390';
               QTD_REG_BLC := Bloco_D.RegistroD390Count;
            end;
         end;
         if Bloco_D.RegistroD400Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'D400';
               QTD_REG_BLC := Bloco_D.RegistroD400Count;
            end;
         end;
         if Bloco_D.RegistroD410Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'D410';
               QTD_REG_BLC := Bloco_D.RegistroD410Count;
            end;
         end;
         if Bloco_D.RegistroD411Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'D411';
               QTD_REG_BLC := Bloco_D.RegistroD411Count;
            end;
         end;
         if Bloco_D.RegistroD420Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'D420';
               QTD_REG_BLC := Bloco_D.RegistroD420Count;
            end;
         end;
         if Bloco_D.RegistroD500Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'D500';
               QTD_REG_BLC := Bloco_D.RegistroD500Count;
            end;
         end;
         if Bloco_D.RegistroD510Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'D510';
               QTD_REG_BLC := Bloco_D.RegistroD510Count;
            end;
         end;
         if Bloco_D.RegistroD530Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'D530';
               QTD_REG_BLC := Bloco_D.RegistroD530Count;
            end;
         end;
         if Bloco_D.RegistroD590Count > 0 then
         begin
            with New do
            begin
              REG_BLC := 'D590';
              QTD_REG_BLC := Bloco_D.RegistroD590Count;
            end;
         end;
         if Bloco_D.RegistroD600Count > 0 then
         begin
            with New do
            begin
              REG_BLC := 'D600';
              QTD_REG_BLC := Bloco_D.RegistroD600Count;
            end;
         end;
         if Bloco_D.RegistroD610Count > 0 then
         begin
            with New do
            begin
              REG_BLC := 'D610';
              QTD_REG_BLC := Bloco_D.RegistroD610Count;
            end;
         end;
         if Bloco_D.RegistroD690Count > 0 then
         begin
            with New do
            begin
              REG_BLC := 'D690';
              QTD_REG_BLC := Bloco_D.RegistroD690Count;
            end;
         end;
         if Bloco_D.RegistroD695Count > 0 then
         begin
            with New do
            begin
              REG_BLC := 'D695';
              QTD_REG_BLC := Bloco_D.RegistroD695Count;
            end;
         end;
         if Bloco_D.RegistroD696Count > 0 then
         begin
            with New do
            begin
              REG_BLC := 'D696';
              QTD_REG_BLC := Bloco_D.RegistroD696Count;
            end;
         end;
         if Bloco_D.RegistroD697Count > 0 then
         begin
            with New do
            begin
              REG_BLC := 'D697';
              QTD_REG_BLC := Bloco_D.RegistroD697Count;
            end;
         end;

         if Bloco_D.RegistroD700Count > 0 then
         begin
           with New do
           begin
             REG_BLC := 'D700';
             QTD_REG_BLC := Bloco_D.RegistroD700Count;
           end;
         end;

         if Bloco_D.RegistroD730Count > 0 then
         begin
           with New do
           begin
             REG_BLC := 'D730';
             QTD_REG_BLC := Bloco_D.RegistroD730Count;
           end;
         end;

         if Bloco_D.RegistroD731Count > 0 then
         begin
           with New do
           begin
             REG_BLC := 'D731';
             QTD_REG_BLC := Bloco_D.RegistroD731Count;
           end;
         end;

         if Bloco_D.RegistroD735Count > 0 then
         begin
           with New do
           begin
             REG_BLC := 'D735';
             QTD_REG_BLC := Bloco_D.RegistroD735Count;
           end;
         end;

         if Bloco_D.RegistroD737Count > 0 then
         begin
           with New do
           begin
             REG_BLC := 'D737';
             QTD_REG_BLC := Bloco_D.RegistroD737Count;
           end;
         end;

         if Bloco_D.RegistroD750Count > 0 then
         begin
           with New do
           begin
             REG_BLC := 'D750';
             QTD_REG_BLC := Bloco_D.RegistroD750Count;
           end;
         end;

         if Bloco_D.RegistroD760Count > 0 then
         begin
           with New do
           begin
             REG_BLC := 'D760';
             QTD_REG_BLC := Bloco_D.RegistroD760Count;
           end;
         end; 

         if Bloco_D.RegistroD761Count > 0 then
         begin
           with New do
           begin
             REG_BLC := 'D761';
             QTD_REG_BLC := Bloco_D.RegistroD761Count;
           end;
         end;

      end;
   end;
end;

procedure TACBrSPEDFiscal.WriteRegistroD990;
begin
  with Bloco_9.Registro9900.New do
  begin
    REG_BLC := 'D990';
    QTD_REG_BLC := 1;
  end;
  Bloco_D.WriteRegistroD990;
end;

procedure TACBrSPEDFiscal.WriteRegistroE001;
begin
   Bloco_E.WriteRegistroE001;
   //
   with Bloco_9.Registro9900 do
   begin
      with New do
      begin
         REG_BLC := 'E001';
         QTD_REG_BLC := 1;
      end;
   end;
   if Bloco_E.RegistroE001.IND_MOV = imComDados then
   begin
      with Bloco_9.Registro9900 do
      begin
         with New do
         begin
            REG_BLC := 'E100';
            QTD_REG_BLC := Bloco_E.RegistroE100Count;
         end;
         with New do
         begin
            REG_BLC := 'E110';
            QTD_REG_BLC := Bloco_E.RegistroE110Count;
         end;
         if Bloco_E.RegistroE111Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'E111';
               QTD_REG_BLC := Bloco_E.RegistroE111Count;
            end;
         end;
         if Bloco_E.RegistroE112Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'E112';
               QTD_REG_BLC := Bloco_E.RegistroE112Count;
            end;
         end;
         if Bloco_E.RegistroE113Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'E113';
               QTD_REG_BLC := Bloco_E.RegistroE113Count;
            end;
         end;
         if Bloco_E.RegistroE115Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'E115';
               QTD_REG_BLC := Bloco_E.RegistroE115Count;
            end;
         end;
         if Bloco_E.RegistroE116Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'E116';
               QTD_REG_BLC := Bloco_E.RegistroE116Count;
            end;
         end;
         if Bloco_E.RegistroE200Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'E200';
               QTD_REG_BLC := Bloco_E.RegistroE200Count;
            end;
         end;
         if Bloco_E.RegistroE210Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'E210';
               QTD_REG_BLC := Bloco_E.RegistroE210Count;
            end;
         end;
         if Bloco_E.RegistroE220Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'E220';
               QTD_REG_BLC := Bloco_E.RegistroE220Count;
            end;
         end;
         if Bloco_E.RegistroE230Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'E230';
               QTD_REG_BLC := Bloco_E.RegistroE230Count;
            end;
         end;
         if Bloco_E.RegistroE240Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'E240';
               QTD_REG_BLC := Bloco_E.RegistroE240Count;
            end;
         end;
         if Bloco_E.RegistroE250Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'E250';
               QTD_REG_BLC := Bloco_E.RegistroE250Count;
            end;
         end;
         if Bloco_E.RegistroE300Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'E300';
               QTD_REG_BLC := Bloco_E.RegistroE300Count;
            end;
         end;
         if Bloco_E.RegistroE310Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'E310';
               QTD_REG_BLC := Bloco_E.RegistroE310Count;
            end;
         end;
         if Bloco_E.RegistroE311Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'E311';
               QTD_REG_BLC := Bloco_E.RegistroE311Count;
            end;
         end;
         if Bloco_E.RegistroE312Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'E312';
               QTD_REG_BLC := Bloco_E.RegistroE312Count;
            end;
         end;
         if Bloco_E.RegistroE313Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'E313';
               QTD_REG_BLC := Bloco_E.RegistroE313Count;
            end;
         end;
         if Bloco_E.RegistroE316Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'E316';
               QTD_REG_BLC := Bloco_E.RegistroE316Count;
            end;
         end;
         if Bloco_E.RegistroE500Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'E500';
               QTD_REG_BLC := Bloco_E.RegistroE500Count;
            end;
         end;
         if Bloco_E.RegistroE510Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'E510';
               QTD_REG_BLC := Bloco_E.RegistroE510Count;
            end;
         end;
         if Bloco_E.RegistroE520Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'E520';
               QTD_REG_BLC := Bloco_E.RegistroE520Count;
            end;
         end;
         if Bloco_E.RegistroE530Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'E530';
               QTD_REG_BLC := Bloco_E.RegistroE530Count;
            end;
         end;
         if Bloco_E.RegistroE531Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'E531';
               QTD_REG_BLC := Bloco_E.RegistroE531Count;
            end;
         end;
      end;
   end;
end;

procedure TACBrSPEDFiscal.WriteRegistroE990;
begin
  with Bloco_9.Registro9900.New do
  begin
    REG_BLC := 'E990';
    QTD_REG_BLC := 1;
  end;
  Bloco_E.WriteRegistroE990;
end;

procedure TACBrSPEDFiscal.WriteRegistroG001;
begin
   Bloco_G.WriteRegistroG001;
   //
   with Bloco_9.Registro9900 do
   begin
      with New do
      begin
         REG_BLC := 'G001';
         QTD_REG_BLC := 1;
      end;
   end;
   if Bloco_G.RegistroG001.IND_MOV = imComDados then
   begin
      with Bloco_9.Registro9900 do
      begin
         if Bloco_G.RegistroG110Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'G110';
               QTD_REG_BLC := Bloco_G.RegistroG110Count;
            end;
         end;
         if Bloco_G.RegistroG125Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'G125';
               QTD_REG_BLC := Bloco_G.RegistroG125Count;
            end;
         end;
         if Bloco_G.RegistroG126Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'G126';
               QTD_REG_BLC := Bloco_G.RegistroG126Count;
            end;
         end;
         if Bloco_G.RegistroG130Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'G130';
               QTD_REG_BLC := Bloco_G.RegistroG130Count;
            end;
         end;
         if Bloco_G.RegistroG140Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'G140';
               QTD_REG_BLC := Bloco_G.RegistroG140Count;
            end;
         end;
      end;
   end;
end;

procedure TACBrSPEDFiscal.WriteRegistroG990;
begin
  with Bloco_9.Registro9900.New do
  begin
    REG_BLC := 'G990';
    QTD_REG_BLC := 1;
  end;
  Bloco_G.WriteRegistroG990;
end;

procedure TACBrSPEDFiscal.WriteRegistroH001;
begin
   Bloco_H.WriteRegistroH001;
   //
   with Bloco_9.Registro9900 do
   begin
      with New do
      begin
         REG_BLC := 'H001';
         QTD_REG_BLC := 1;
      end;
   end;
   if Bloco_H.RegistroH001.IND_MOV = imComDados then
   begin
      with Bloco_9.Registro9900 do
      begin
         if Bloco_H.RegistroH005Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'H005';
               QTD_REG_BLC := Bloco_H.RegistroH005Count;
            end;
         end;
         if Bloco_H.RegistroH010Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'H010';
               QTD_REG_BLC := Bloco_H.RegistroH010Count;
            end;
         end;
         if Bloco_H.RegistroH020Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'H020';
               QTD_REG_BLC := Bloco_H.RegistroH020Count;
            end;
         end;
         if Bloco_H.RegistroH030Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'H030';
               QTD_REG_BLC := Bloco_H.RegistroH030Count;
            end;
         end;
      end;
   end;
end;

procedure TACBrSPEDFiscal.WriteRegistroH990;
begin
  with Bloco_9.Registro9900.New do
  begin
    REG_BLC := 'H990';
    QTD_REG_BLC := 1;
  end;
  Bloco_H.WriteRegistroH990;
end;

procedure TACBrSPEDFiscal.WriteRegistroK001;
begin
  Bloco_K.WriteRegistroK001;
   //
   with Bloco_9.Registro9900 do
   begin
      with New do
      begin
         REG_BLC := 'K001';
         QTD_REG_BLC := 1;
      end;
   end;
   if Bloco_K.RegistroK001.IND_MOV = imComDados then
   begin
      if (Bloco_0.Registro0000.COD_VER >= vlVersao116) then
      with Bloco_9.Registro9900.New do
      begin
        REG_BLC     := 'K010';
        QTD_REG_BLC := 1;
      end;
        
      with Bloco_9.Registro9900 do
      begin
         if Bloco_K.RegistroK100Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'K100';
               QTD_REG_BLC := Bloco_K.RegistroK100Count;
            end;
         end;
         if Bloco_K.RegistroK200Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'K200';
               QTD_REG_BLC := Bloco_K.RegistroK200Count;
            end;
         end;
         if Bloco_K.RegistroK210Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'K210';
               QTD_REG_BLC := Bloco_K.RegistroK210Count;
            end;
         end;
         if Bloco_K.RegistroK215Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'K215';
               QTD_REG_BLC := Bloco_K.RegistroK215Count;
            end;
         end;
         if Bloco_K.RegistroK220Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'K220';
               QTD_REG_BLC := Bloco_K.RegistroK220Count;
            end;
         end;
         if Bloco_K.RegistroK230Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'K230';
               QTD_REG_BLC := Bloco_K.RegistroK230Count;
            end;
         end;
         if Bloco_K.RegistroK235Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'K235';
               QTD_REG_BLC := Bloco_K.RegistroK235Count;
            end;
         end;
         if Bloco_K.RegistroK250Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'K250';
               QTD_REG_BLC := Bloco_K.RegistroK250Count;
            end;
         end;
         if Bloco_K.RegistroK255Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'K255';
               QTD_REG_BLC := Bloco_K.RegistroK255Count;
            end;
         end;
         if Bloco_K.RegistroK260Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'K260';
               QTD_REG_BLC := Bloco_K.RegistroK260Count;
            end;
         end;
         if Bloco_K.RegistroK265Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'K265';
               QTD_REG_BLC := Bloco_K.RegistroK265Count;
            end;
         end;
         if Bloco_K.RegistroK270Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'K270';
               QTD_REG_BLC := Bloco_K.RegistroK270Count;
            end;
         end;
         if Bloco_K.RegistroK275Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'K275';
               QTD_REG_BLC := Bloco_K.RegistroK275Count;
            end;
         end;
         if Bloco_K.RegistroK280Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'K280';
               QTD_REG_BLC := Bloco_K.RegistroK280Count;
            end;
         end;
         if Bloco_K.RegistroK290Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'K290';
               QTD_REG_BLC := Bloco_K.RegistroK290Count;
            end;
         end;
         if Bloco_K.RegistroK291Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'K291';
               QTD_REG_BLC := Bloco_K.RegistroK291Count;
            end;
         end;
         if Bloco_K.RegistroK292Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'K292';
               QTD_REG_BLC := Bloco_K.RegistroK292Count;
            end;
         end;
         if Bloco_K.RegistroK300Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'K300';
               QTD_REG_BLC := Bloco_K.RegistroK300Count;
            end;
         end;
         if Bloco_K.RegistroK301Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'K301';
               QTD_REG_BLC := Bloco_K.RegistroK301Count;
            end;
         end;
         if Bloco_K.RegistroK302Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'K302';
               QTD_REG_BLC := Bloco_K.RegistroK302Count;
            end;
         end;
      end;
   end;
end;

procedure TACBrSPEDFiscal.WriteRegistroK990;
begin
  with Bloco_9.Registro9900.New do
  begin
    REG_BLC := 'K990';
    QTD_REG_BLC := 1;
  end;
  Bloco_K.WriteRegistroK990;
end;

procedure TACBrSPEDFiscal.WriteRegistro9001;
begin
  with Bloco_9.Registro9900.New do
  begin
    REG_BLC := '9001';
    QTD_REG_BLC := 1;
  end;
  Bloco_9.WriteRegistro9001;
end;

procedure TACBrSPEDFiscal.WriteRegistro9900;
begin
  with Bloco_9.Registro9900 do
  begin
    with New do
    begin
      REG_BLC := '9900';
      QTD_REG_BLC := Bloco_9.Registro9900.Count + 2;
    end;
    with New do
    begin
      REG_BLC := '9990';
      QTD_REG_BLC := 1;
    end;
    with New do
    begin
      REG_BLC := '9999';
      QTD_REG_BLC := 1;
    end;
  end;
  Bloco_9.WriteRegistro9900;
end;

procedure TACBrSPEDFiscal.WriteRegistro9990;
begin
  Bloco_9.WriteRegistro9990;
end;

procedure TACBrSPEDFiscal.WriteRegistro9999;
begin
  Bloco_9.Registro9999.QTD_LIN := Bloco_9.Registro9999.QTD_LIN + Bloco_0.Registro0990.QTD_LIN_0 +
                                                                 Bloco_1.Registro1990.QTD_LIN_1 +
                                                                 ifthen(Bloco_B.DT_INI >= EncodeDate(2019,01,01), Bloco_B.RegistroB990.QTD_LIN_B, 0) +
                                                                 Bloco_C.RegistroC990.QTD_LIN_C +
                                                                 Bloco_D.RegistroD990.QTD_LIN_D +
                                                                 Bloco_E.RegistroE990.QTD_LIN_E +
                                                                 ifThen(Bloco_G.DT_INI >= EncodeDate(2011,01,01), Bloco_G.RegistroG990.QTD_LIN_G, 0) +
                                                                 ifthen(Bloco_K.DT_INI >= EncodeDate(2015,01,01), Bloco_K.RegistroK990.QTD_LIN_K, 0) +
                                                                 Bloco_H.RegistroH990.QTD_LIN_H +
                                                                 Bloco_9.Registro9990.QTD_LIN_9;
  Bloco_9.WriteRegistro9999;
end;

procedure TACBrSPEDFiscal.WriteRegistroB001;
begin
  Bloco_B.WriteRegistroB001;
  //
  with Bloco_9.Registro9900 do
  begin
    with New do
    begin
       REG_BLC := 'B001';
       QTD_REG_BLC := 1;
    end;
  end;
  if Bloco_B.RegistroB001.IND_MOV = imComDados then
  begin
    with Bloco_9.Registro9900 do
    begin
      if Bloco_B.RegistroB020Count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'B020';
          QTD_REG_BLC := Bloco_B.RegistroB020Count;
        end;
      end;
      if Bloco_B.RegistroB025Count > 0 then
      begin
       with New do
       begin
         REG_BLC := 'B025';
         QTD_REG_BLC := Bloco_B.RegistroB025Count;
       end;
      end;
      if Bloco_B.RegistroB030Count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'B030';
          QTD_REG_BLC := Bloco_B.RegistroB030Count;
        end;
      end;
      if Bloco_B.RegistroB035Count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'B035';
          QTD_REG_BLC := Bloco_B.RegistroB035Count;
        end;
      end;
      if Bloco_B.RegistroB350Count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'B350';
          QTD_REG_BLC := Bloco_B.RegistroB350Count;
        end;
      end;
      if Bloco_B.RegistroB420Count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'B420';
          QTD_REG_BLC := Bloco_B.RegistroB420Count;
        end;
      end;
      if Bloco_B.RegistroB440Count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'B440';
          QTD_REG_BLC := Bloco_B.RegistroB440Count;
        end;
      end;
      if Bloco_B.RegistroB460count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'B460';
          QTD_REG_BLC := Bloco_B.RegistroB460Count;
        end;
      end;
      if Bloco_B.RegistroB470Count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'B470';
          QTD_REG_BLC := Bloco_B.RegistroB470Count;
        end;
      end;
      if Bloco_B.RegistroB500Count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'B500';
          QTD_REG_BLC := Bloco_B.RegistroB500Count;
        end;
      end;
      if Bloco_B.RegistroB510Count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'B510';
          QTD_REG_BLC := Bloco_B.RegistroB510Count;
        end;
      end;
    end;
  end;
end;

procedure TACBrSPEDFiscal.WriteRegistroB990;
begin
  with Bloco_9.Registro9900.New do
  begin
    REG_BLC := 'B990';
    QTD_REG_BLC := 1;
  end;
  Bloco_B.WriteRegistroB990;
end;

{$IFNDEF Framework}
{$IFDEF FPC}
initialization
   {$I ACBrSpedFiscal.lrs}
{$ENDIF}
{$ENDIF}

end.

