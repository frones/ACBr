{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2014   Juliomar Marchetti                   }
{					                    2015   Isaque Pinheiro	    	             }
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
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }
{                                                                              }
{******************************************************************************}

{******************************************************************************
|* Historico
|*
*******************************************************************************}

{$I ACBr.inc}

unit ACBrSpedECF;

interface

uses
  SysUtils, Math, Classes,
{$IFNDEF Framework}
  {$IFDEF FPC}
    LResources,
  {$ENDIF}
{$ENDIF}
  DateUtils, ACBrSped, ACBrTXTClass, ACBrECFBlocos,
  ACBrECFBloco_0_Class, ACBrECFBloco_C_Class, ACBrECFBloco_E_Class,
  ACBrECFBloco_J_Class, ACBrECFBloco_K_Class, ACBrECFBloco_L_Class,
  ACBrECFBloco_M_Class, ACBrECFBloco_N_Class, ACBrECFBloco_P_Class,
  ACBrECFBloco_T_Class, ACBrECFBloco_U_Class, ACBrECFBloco_X_Class,
  ACBrECFBloco_Y_Class, ACBrECFBloco_9_Class ;

const
  CACBrSpedECF_Versao = '0.01';

type
  /// ACBrSpedECF - Sitema Publico de Escrituração Contábil Fiscal

  { TACBrSPEDECF }

  TACBrSPEDECF = class(TComponent)
  private
    FACBrTXT: TACBrTXTClass;
    FArquivo: ansistring;
    FInicializado : boolean;
    FOnError: TErrorEvent;
    FDT_INI: TDateTime;
    FDT_FIN: TDateTime;
    FPath: ansistring;
    FDelimitador: ansistring;
    FTrimString: boolean;
    FCurMascara: ansistring;

    FBloco_0: TBloco_0;
    FBloco_C: TBloco_C;
    FBloco_E: TBloco_E;
    FBloco_J: TBloco_J;
    FBloco_K: TBloco_K;
    FBloco_L: TBloco_L;
    FBloco_M: TBloco_M;
    FBloco_N: TBloco_N;
    FBloco_P: TBloco_P;
    FBloco_T: TBloco_T;
    FBloco_U: TBloco_U;
    FBloco_X: TBloco_X;
    FBloco_Y: TBloco_Y;
    FBloco_9: TBloco_9;

    function GetAbout: ansistring;
    function GetConteudo: TStringList;
    function GetDelimitador: ansistring;
    function GetLinhasBuffer: Integer;
    function GetTrimString: boolean;
    function GetCurMascara: ansistring;
    function GetDT_FIN: TDateTime;
    function GetDT_INI: TDateTime;
    procedure InicializaBloco(Bloco: TACBrSPED);
    procedure SetArquivo(const Value: ansistring);
    procedure SetDelimitador(const Value: ansistring);
    procedure SetLinhasBuffer(const Value: Integer);
    procedure SetPath(const Value: ansistring);
    procedure SetTrimString(const Value: boolean);
    procedure SetCurMascara(const Value: ansistring);
    procedure SetDT_FIN(const Value: TDateTime);
    procedure SetDT_INI(const Value: TDateTime);

    function GetOnError: TErrorEvent; /// Método do evento OnError
    procedure SetOnError(const Value: TErrorEvent); /// Método SetError

    procedure LimpaRegistros;
  protected
    /// BLOCO 0		
    procedure WriteRegistro0000;
    procedure WriteRegistro0001;
    procedure WriteRegistro0990;
    /// BLOCO C
    procedure WriteRegistroC001;
    procedure WriteRegistroC990;
    /// BLOCO E
    procedure WriteRegistroE001;
    procedure WriteRegistroE990;
    /// BLOCO J
    procedure WriteRegistroJ001;
    procedure WriteRegistroJ990;
    /// BLOCO K
    procedure WriteRegistroK001;
    procedure WriteRegistroK990;
		/// BLOCO L
    procedure WriteRegistroL001;
    procedure WriteRegistroL990;
		/// BLOCO M
    procedure WriteRegistroM001;
    procedure WriteRegistroM990;
		/// BLOCO N
    procedure WriteRegistroN001;
    procedure WriteRegistroN990;
		/// BLOCO P
    procedure WriteRegistroP001;
    procedure WriteRegistroP990;
		/// BLOCO T
    procedure WriteRegistroT001;
    procedure WriteRegistroT990;
		/// BLOCO U
    procedure WriteRegistroU001;
    procedure WriteRegistroU990;
		/// BLOCO X
    procedure WriteRegistroX001;
    procedure WriteRegistroX990;
		/// BLOCO Y
    procedure WriteRegistroY001;
    procedure WriteRegistroY990;
    /// BLOCO 9
    procedure WriteRegistro9001;
    procedure WriteRegistro9900;
    procedure WriteRegistro9990;
    procedure WriteRegistro9999;
  public
    constructor Create(AOwner: TComponent); override; /// Create
    destructor Destroy; override; /// Destroy
    procedure SaveFileTXT;
		
    procedure IniciaGeracao;
    procedure CancelaGeracao;
		
    procedure WriteBloco_0;
    procedure WriteBloco_C;
    procedure WriteBloco_E;
    procedure WriteBloco_J;
    procedure WriteBloco_K;
    procedure WriteBloco_L;
    procedure WriteBloco_M;
    procedure WriteBloco_N;
    procedure WriteBloco_P;
    procedure WriteBloco_T;
    procedure WriteBloco_U;
    procedure WriteBloco_X;
    procedure WriteBloco_Y;
    procedure WriteBloco_9;
		
    property Conteudo: TStringList read GetConteudo;

    property DT_INI: TDateTime read GetDT_INI write SetDT_INI;
    property DT_FIN: TDateTime read GetDT_FIN write SetDT_FIN;

    property Bloco_0: TBloco_0 read FBloco_0 write FBloco_0;
    property Bloco_C: TBloco_C read FBloco_C write FBloco_C;
    property Bloco_E: TBloco_E read FBloco_E write FBloco_E;
    property Bloco_J: TBloco_J read FBloco_J write FBloco_J;
    property Bloco_K: TBloco_K read FBloco_K write FBloco_K;
    property Bloco_L: TBloco_L read FBloco_L write FBloco_L;
    property Bloco_M: TBloco_M read FBloco_M write FBloco_M;
    property Bloco_N: TBloco_N read FBloco_N write FBloco_N;
    property Bloco_P: TBloco_P read FBloco_P write FBloco_P;
    property Bloco_T: TBloco_T read FBloco_T write FBloco_T;
    property Bloco_U: TBloco_U read FBloco_U write FBloco_U;
    property Bloco_X: TBloco_X read FBloco_X write FBloco_X;
    property Bloco_Y: TBloco_Y read FBloco_Y write FBloco_Y;
    property Bloco_9: TBloco_9 read FBloco_9 write FBloco_9;

  published
    property About: ansistring read GetAbout stored False;
    property Path: ansistring read FPath write SetPath;
    property Arquivo: ansistring read FArquivo write SetArquivo;
    property LinhasBuffer : Integer read GetLinhasBuffer write SetLinhasBuffer default 1000 ;
    property Delimitador: ansistring read GetDelimitador write SetDelimitador;
    property TrimString: boolean read GetTrimString write SetTrimString;
    property CurMascara: ansistring read GetCurMascara write SetCurMascara;

    property OnError: TErrorEvent read GetOnError write SetOnError;

  end;

procedure Register;

implementation

uses ACBrUtil;

{$IFNDEF FPC}
 {$R ACBr_SPEDECF.dcr}
{$ENDIF}

procedure Register;
begin
  RegisterComponents('ACBrTXT', [TACBrSPEDECF]);
end;

(* TACBrSpedECF *)

procedure TACBrSPEDECF.CancelaGeracao;
begin
  LimpaRegistros;
  FInicializado := False;
end;

constructor TACBrSPEDECF.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FACBrTXT := TACBrTXTClass.Create;
  FACBrTXT.LinhasBuffer := 1000 ;

  FInicializado := False;

  FBloco_0 := TBloco_0.Create;
  FBloco_C := TBloco_C.Create;
  FBloco_E := TBloco_E.Create;
  FBloco_J := TBloco_J.Create;
  FBloco_K := TBloco_K.Create;
  FBloco_L := TBloco_L.Create;
  FBloco_M := TBloco_M.Create;
  FBloco_N := TBloco_N.Create;
  FBloco_P := TBloco_P.Create;
  FBloco_T := TBloco_T.Create;
  FBloco_U := TBloco_U.Create;
  FBloco_X := TBloco_X.Create;
  FBloco_Y := TBloco_Y.Create;
  FBloco_9 := TBloco_9.Create;

  /// Objeto passado por referência para que possamos usa-lo para fazer pesquisa
  /// em seus registros.
  FBloco_C.Bloco_0 := FBloco_0;
  FBloco_E.Bloco_0 := FBloco_0;
  FBloco_J.Bloco_0 := FBloco_0;
  FBloco_K.Bloco_0 := FBloco_0;
  FBloco_L.Bloco_0 := FBloco_0;
  FBloco_M.Bloco_0 := FBloco_0;
  FBloco_N.Bloco_0 := FBloco_0;
  FBloco_P.Bloco_0 := FBloco_0;
  FBloco_T.Bloco_0 := FBloco_0;
  FBloco_U.Bloco_0 := FBloco_0;
  FBloco_X.Bloco_0 := FBloco_0;
  FBloco_Y.Bloco_0 := FBloco_0;

  FPath := ExtractFilePath(ParamStr(0));
  FDelimitador := '|';
  FCurMascara := '#0.00';
  FTrimString := True;

  SetDelimitador(FDelimitador);
  SetCurMascara(FCurMascara);
  SetTrimString(FTrimString);
end;

destructor TACBrSPEDECF.Destroy;
begin
  FACBrTXT.Free;

  FBloco_0.Free;
  FBloco_C.Free;
  FBloco_E.Free;
  FBloco_J.Free;
  FBloco_K.Free;
  FBloco_L.Free;
  FBloco_M.Free;
  FBloco_N.Free;
  FBloco_P.Free;
  FBloco_T.Free;
  FBloco_U.Free;
  FBloco_X.Free;
  FBloco_Y.Free;
  FBloco_9.Free;
  inherited;
end;

procedure TACBrSPEDECF.LimpaRegistros;
begin
  FBloco_0.LimpaRegistros;
  FBloco_C.LimpaRegistros;
  FBloco_E.LimpaRegistros;
  FBloco_J.LimpaRegistros;
  FBloco_K.LimpaRegistros;
  FBloco_L.LimpaRegistros;
  FBloco_M.LimpaRegistros;
  FBloco_N.LimpaRegistros;
  FBloco_P.LimpaRegistros;
  FBloco_T.LimpaRegistros;
  FBloco_U.LimpaRegistros;
  FBloco_X.LimpaRegistros;
  FBloco_Y.LimpaRegistros;
  FBloco_9.LimpaRegistros;
end;

function TACBrSPEDECF.GetAbout: ansistring;
begin
  Result := Format('ACBrSpedECF Ver: %s ', [CACBrSpedECF_Versao]);
end;

function TACBrSPEDECF.GetConteudo: TStringList;
begin
  Result := FACBrTXT.Conteudo;
end;

function TACBrSPEDECF.GetDelimitador: ansistring;
begin
   Result := FDelimitador;
end;

function TACBrSPEDECF.GetLinhasBuffer: Integer;
begin
   Result := FACBrTXT.LinhasBuffer ;
end;

procedure TACBrSPEDECF.SetDelimitador(const Value: ansistring);
begin
  if Value = '' then
     raise Exception.Create('Campo não pode ser vazio!');

  FDelimitador := Value;
	
  FBloco_0.Delimitador := Value;
  FBloco_C.Delimitador := Value;
  FBloco_E.Delimitador := Value;
  FBloco_J.Delimitador := Value;
  FBloco_K.Delimitador := Value;
  FBloco_L.Delimitador := Value;
  FBloco_M.Delimitador := Value;
  FBloco_N.Delimitador := Value;
  FBloco_P.Delimitador := Value;
  FBloco_T.Delimitador := Value;
  FBloco_U.Delimitador := Value;
  FBloco_X.Delimitador := Value;
  FBloco_Y.Delimitador := Value;
  FBloco_9.Delimitador := Value;
end;

procedure TACBrSPEDECF.SetLinhasBuffer(const Value: Integer);
begin
   FACBrTXT.LinhasBuffer := Value ;
end;

procedure TACBrSPEDECF.SetPath(const Value: ansistring);
begin
  if Value = '' then
     raise Exception.Create('Campo não pode ser vazio!');

  FPath := PathWithDelim( Value );
end;

function TACBrSPEDECF.GetCurMascara: ansistring;
begin
  Result := FCurMascara;
end;

procedure TACBrSPEDECF.SetCurMascara(const Value: ansistring);
begin
  if Value = '' then
     raise Exception.Create('Campo não pode ser vazio! Para deixar sem mascara digite #');

  FCurMascara := Value;

  FBloco_0.CurMascara := Value;
  FBloco_C.CurMascara := Value;
  FBloco_E.CurMascara := Value;
  FBloco_J.CurMascara := Value;
  FBloco_K.CurMascara := Value;
  FBloco_L.CurMascara := Value;
  FBloco_M.CurMascara := Value;
  FBloco_N.CurMascara := Value;
  FBloco_P.CurMascara := Value;
  FBloco_T.CurMascara := Value;
  FBloco_U.CurMascara := Value;
  FBloco_X.CurMascara := Value;
  FBloco_Y.CurMascara := Value;
  FBloco_9.CurMascara := Value;
end;

function TACBrSPEDECF.GetTrimString: boolean;
begin
  Result := FTrimString;
end;

procedure TACBrSPEDECF.SetTrimString(const Value: boolean);
begin
  FTrimString := Value;

  FBloco_0.TrimString := Value;
  FBloco_C.TrimString := Value;
  FBloco_E.TrimString := Value;
  FBloco_J.TrimString := Value;
  FBloco_K.TrimString := Value;
  FBloco_L.TrimString := Value;
  FBloco_M.TrimString := Value;
  FBloco_N.TrimString := Value;
  FBloco_P.TrimString := Value;
  FBloco_T.TrimString := Value;
  FBloco_U.TrimString := Value;
  FBloco_X.TrimString := Value;
  FBloco_Y.TrimString := Value;
  FBloco_9.TrimString := Value;
end;

function TACBrSPEDECF.GetDT_INI: TDateTime;
begin
  Result := FDT_INI;
end;

procedure TACBrSPEDECF.InicializaBloco( Bloco: TACBrSPED ) ;
begin
  Bloco.NomeArquivo  := FACBrTXT.NomeArquivo;
  Bloco.LinhasBuffer := FACBrTXT.LinhasBuffer;
  Bloco.Gravado      := False ;
  if not assigned(Bloco.Conteudo) then
    Bloco.Conteudo := TStringList.Create;
  Bloco.Conteudo.Clear;
end;

procedure TACBrSPEDECF.IniciaGeracao;
begin
  if FInicializado then exit ;

  if (Trim(FArquivo) = '') or (Trim(FPath) = '') then
    raise Exception.Create(ACBrStr('Caminho ou nome do arquivo não informado!'));

  FACBrTXT.NomeArquivo := FPath + FArquivo ;
  FACBrTXT.Reset;    // Apaga o Arquivo e limpa memória

  InicializaBloco( Bloco_0 );
  InicializaBloco( Bloco_C );
  InicializaBloco( Bloco_E );
  InicializaBloco( Bloco_J );
  InicializaBloco( Bloco_K );
  InicializaBloco( Bloco_L );
  InicializaBloco( Bloco_M );
  InicializaBloco( Bloco_N );
  InicializaBloco( Bloco_P );
  InicializaBloco( Bloco_T );
  InicializaBloco( Bloco_U );
  InicializaBloco( Bloco_X );
  InicializaBloco( Bloco_Y );
  InicializaBloco( Bloco_9 );

  ///
  FACBrTXT.Check(FDT_INI > 0, 'CHECAGEM INICIAL: Informe a data '
    + 'inicial das informações contidas no arquivo!');
  FACBrTXT.Check(FDT_FIN > 0, 'CHECAGEM INICIAL: Informe a data '
    + 'final das informações contidas no arquivo!');
  FACBrTXT.Check(DayOf(FDT_INI) = 1, 'CHECAGEM INICIAL: A data inicial deve '
    + 'corresponder ao primeiro dia do mês informado!');
  FACBrTXT.Check(FDT_FIN >= FDT_INI, 'CHECAGEM INICIAL: A data final deve se '
    + 'maior que a data inicial!');
  FACBrTXT.Check(FDT_FIN <= Date, 'CHECAGEM INICIAL: A data final "%s" '
    + 'não pode ser superior a data atual "%s"!',
    [DateToStr(FDT_FIN), DateToStr(Date)]);
  FACBrTXT.Check(DateOf(EndOfTheMonth(FDT_FIN)) = DateOf(FDT_FIN),
    'CHECAGEM ' + 'INICIAL: A data final deve corresponder ao último dia do mês '
    + 'informado!');

  /// Preparação para totalizações de registros.
  Bloco_0.Registro0990.QTD_LIN := 0;
  Bloco_C.RegistroC990.QTD_LIN := 0;
  Bloco_E.RegistroE990.QTD_LIN := 0;
  Bloco_J.RegistroJ990.QTD_LIN := 0;
  Bloco_K.RegistroK990.QTD_LIN := 0;
  Bloco_L.RegistroL990.QTD_LIN := 0;
  Bloco_M.RegistroM990.QTD_LIN := 0;
  Bloco_N.RegistroN990.QTD_LIN := 0;
  Bloco_P.RegistroP990.QTD_LIN := 0;
  Bloco_T.RegistroT990.QTD_LIN := 0;
  Bloco_U.RegistroU990.QTD_LIN := 0;
  Bloco_X.RegistroX990.QTD_LIN := 0;
  Bloco_Y.RegistroY990.QTD_LIN := 0;
  Bloco_9.Registro9990.QTD_LIN := 0;
  Bloco_9.Registro9999.QTD_LIN := 0;
  /// Limpa a lista
  Bloco_9.Registro9900.Clear;
  FInicializado := True ;
end;

procedure TACBrSPEDECF.SetArquivo(const Value: ansistring);
var
  APath : AnsiString;
begin
  if FArquivo = Value then
     exit;
  FArquivo := ExtractFileName( Value );
  APath    := ExtractFilePath( Value );
  if APath <> '' then
     Path := APath;
end;

procedure TACBrSPEDECF.SetDT_INI(const Value: TDateTime);
begin
  FDT_INI := Value;

  FBloco_0.DT_INI := Value;
  FBloco_C.DT_INI := Value;
  FBloco_E.DT_INI := Value;
  FBloco_J.DT_INI := Value;
  FBloco_K.DT_INI := Value;
  FBloco_L.DT_INI := Value;
  FBloco_M.DT_INI := Value;
  FBloco_N.DT_INI := Value;
  FBloco_P.DT_INI := Value;
  FBloco_T.DT_INI := Value;
  FBloco_U.DT_INI := Value;
  FBloco_X.DT_INI := Value;
  FBloco_Y.DT_INI := Value;
  FBloco_9.DT_INI := Value;

  if Assigned(FBloco_0) then
    FBloco_0.Registro0000.DT_INI := Value;
end;

function TACBrSPEDECF.GetDT_FIN: TDateTime;
begin
  Result := FDT_FIN;
end;

procedure TACBrSPEDECF.SetDT_FIN(const Value: TDateTime);
begin
  FDT_FIN := Value;

  FBloco_0.DT_FIN := Value;
  FBloco_C.DT_FIN := Value;
  FBloco_E.DT_FIN := Value;
  FBloco_J.DT_FIN := Value;
  FBloco_K.DT_FIN := Value;
  FBloco_L.DT_FIN := Value;
  FBloco_M.DT_FIN := Value;
  FBloco_N.DT_FIN := Value;
  FBloco_P.DT_FIN := Value;
  FBloco_T.DT_FIN := Value;
  FBloco_U.DT_FIN := Value;
  FBloco_X.DT_FIN := Value;
  FBloco_Y.DT_FIN := Value;
  FBloco_9.DT_FIN := Value;
  if Assigned(FBloco_0) then
    FBloco_0.Registro0000.DT_FIN := Value;
end;

function TACBrSPEDECF.GetOnError: TErrorEvent;
begin
  Result := FOnError;
end;

procedure TACBrSPEDECF.SetOnError(const Value: TErrorEvent);
begin
  FOnError := Value;

  FBloco_0.OnError := Value;
  FBloco_C.OnError := Value;
  FBloco_E.OnError := Value;
  FBloco_J.OnError := Value;
  FBloco_K.OnError := Value;
  FBloco_L.OnError := Value;
  FBloco_M.OnError := Value;
  FBloco_N.OnError := Value;
  FBloco_P.OnError := Value;
  FBloco_T.OnError := Value;
  FBloco_U.OnError := Value;
  FBloco_X.OnError := Value;
  FBloco_Y.OnError := Value;
  FBloco_9.OnError := Value;
end;

procedure TACBrSPEDECF.SaveFileTXT;
begin
  try
    IniciaGeracao;

    WriteBloco_0;
    WriteBloco_C;
    WriteBloco_E;
    WriteBloco_J;
    WriteBloco_K;
    WriteBloco_L;
    WriteBloco_M;
    WriteBloco_N;
    WriteBloco_P;
    WriteBloco_T;
    WriteBloco_U;
    WriteBloco_X;
    WriteBloco_Y;
    WriteBloco_9;

  finally
    LimpaRegistros;
    FACBrTXT.Conteudo.Clear;
    FInicializado := False ;
  end;
end;

procedure TACBrSPEDECF.WriteBloco_0;
begin
  if Bloco_0.Gravado then exit ;

  if not FInicializado then
     raise Exception.Create( 'Métodos "IniciaGeracao" não foi executado' );

  // BLOCO 0
  WriteRegistro0000;
  WriteRegistro0001;
  WriteRegistro0990;
  Bloco_0.WriteBuffer;
  Bloco_0.Conteudo.Clear;
  Bloco_0.Gravado := True;
end;

procedure TACBrSPEDECF.WriteBloco_C;
begin
   if Bloco_C.Gravado then exit ;

   if not Bloco_0.Gravado then
      WriteBloco_0 ;

   /// BLOCO C
   WriteRegistroC001;

   //if Bloco_C.RegistroC001.IND_MOV = imSemDados then
      //FechaBloco := True ;

   //if FechaBloco then
      //WriteRegistroC990;

   Bloco_C.WriteBuffer;
   Bloco_C.Conteudo.Clear;

   //Bloco_C.Gravado := FechaBloco;
end;

procedure TACBrSPEDECF.WriteBloco_E;
begin
   if Bloco_E.Gravado then exit ;

   if not Bloco_C.Gravado then
      WriteBloco_C;

   /// BLOCO E
   WriteRegistroE001;
   WriteRegistroE990;
   Bloco_E.WriteBuffer;
   Bloco_E.Conteudo.Clear;
   Bloco_E.Gravado := True ;
end;

procedure TACBrSPEDECF.WriteBloco_J;
begin
   if Bloco_J.Gravado then exit ;

   if not Bloco_J.Gravado then
      WriteBloco_E;

     //WriteRegistroG001;
     //WriteRegistroG990;
     //Bloco_G.WriteBuffer;

   Bloco_J.Conteudo.Clear;
   Bloco_J.Gravado := True ;
end;

procedure TACBrSPEDECF.WriteBloco_N;
begin
   if Bloco_N.Gravado then exit ;

   if not Bloco_M.Gravado then
      WriteBloco_M;

   /// BLOCO N
   //WriteRegistroH001;
   //WriteRegistroH990;
   //Bloco_H.WriteBuffer;
   //Bloco_H.Conteudo.Clear;
   //Bloco_H.Gravado := True ;
end;

procedure TACBrSPEDECF.WriteBloco_P;
begin
   if Bloco_P.Gravado then exit ;

   if not Bloco_N.Gravado then
      WriteBloco_N;

   /// BLOCO P
   //WriteRegistroP001;
   //WriteRegistroP990;
   //Bloco_P.WriteBuffer;
   //Bloco_P.Conteudo.Clear;
   //Bloco_P.Gravado := True ;
end;

procedure TACBrSPEDECF.WriteBloco_T;
begin
   if Bloco_T.Gravado then exit ;

   if not Bloco_P.Gravado then
      WriteBloco_P;

   /// BLOCO T
   //WriteRegistroT001;
   //WriteRegistroT990;
   //Bloco_T.WriteBuffer;
   //Bloco_T.Conteudo.Clear;
   //Bloco_T.Gravado := True ;
end;

procedure TACBrSPEDECF.WriteBloco_U;
begin
   if Bloco_U.Gravado then exit ;

   if not Bloco_T.Gravado then
      WriteBloco_T;

   /// BLOCO U
   //WriteRegistroU001;
   //WriteRegistroU990;
   //Bloco_U.WriteBuffer;
   //Bloco_U.Conteudo.Clear;
   //Bloco_U.Gravado := True ;
end;

procedure TACBrSPEDECF.WriteBloco_X;
begin
   if Bloco_X.Gravado then exit ;

   if not Bloco_U.Gravado then
      WriteBloco_U;

   /// BLOCO X
   //WriteRegistroX001;
   //WriteRegistroX990;
   //Bloco_X.WriteBuffer;
   //Bloco_X.Conteudo.Clear;
   //Bloco_X.Gravado := True ;
end;

procedure TACBrSPEDECF.WriteBloco_Y;
begin
   if Bloco_Y.Gravado then exit ;

   if not Bloco_X.Gravado then
      WriteBloco_X;

   /// BLOCO Y
   //WriteRegistroY001;
   //WriteRegistroY990;
   //Bloco_Y.WriteBuffer;
   //Bloco_Y.Conteudo.Clear;
   //Bloco_Y.Gravado := True ;
end;

procedure TACBrSPEDECF.WriteBloco_K;
begin
   if Bloco_K.Gravado then exit ;
//
//   if not Bloco_H.Gravado then
//      WriteBloco_H;
//
//   /// Alteração da minuta que terá validada a partir de 01 de janeiro de 2015
//   if DT_INI >= EncodeDate(2015,01,01) then
//   begin
//     /// BLOCO K
//     WriteRegistroK001;
//     WriteRegistroK990;
//     Bloco_K.WriteBuffer;
//   end;
   Bloco_K.Conteudo.Clear;
   Bloco_K.Gravado := True ;
end;

procedure TACBrSPEDECF.WriteBloco_L;
begin
   if Bloco_L.Gravado then exit ;
//
//   if not Bloco_H.Gravado then
//      WriteBloco_H;
//
//   /// Alteração da minuta que terá validada a partir de 01 de janeiro de 2015
//   if DT_INI >= EncodeDate(2015,01,01) then
//   begin
//     /// BLOCO K
//     WriteRegistroK001;
//     WriteRegistroK990;
//     Bloco_K.WriteBuffer;
//   end;
   Bloco_L.Conteudo.Clear;
   Bloco_L.Gravado := True ;
end;

procedure TACBrSPEDECF.WriteBloco_M;
begin
   if Bloco_M.Gravado then exit ;

   if not Bloco_L.Gravado then
      WriteBloco_L;

   /// BLOCO M
   //WriteRegistro1001;
   //WriteRegistro1990;
   //Bloco_M.WriteBuffer;
   //Bloco_M.Conteudo.Clear;
   //Bloco_M.Gravado := True ;
end;

procedure TACBrSPEDECF.WriteBloco_9;
begin
   if Bloco_9.Gravado then exit ;

   if not Bloco_Y.Gravado then
      WriteBloco_Y ;

   /// BLOCO 9
   WriteRegistro9001;
   WriteRegistro9900;
   WriteRegistro9990;
   WriteRegistro9999;
   Bloco_9.WriteBuffer;
   Bloco_9.Conteudo.Clear;
   Bloco_9.Gravado := True ;
end;

procedure TACBrSPEDECF.WriteRegistro0000;
begin
   with Bloco_9.Registro9900.New do
   begin
      REG_BLC := '0000';
      QTD_REG_BLC := 1;
   end;
   Bloco_0.WriteRegistro0000;
end;

procedure TACBrSPEDECF.WriteRegistro0001;
begin
   // Preenche as classes com os dados
   Bloco_0.WriteRegistro0001;

   with Bloco_9.Registro9900 do
   begin
      with New do
      begin
         REG_BLC := '0001';
         QTD_REG_BLC := 1;
      end;
   end;
   if Bloco_0.Registro0001.IND_DAD = idComDados then
   begin
      with Bloco_9.Registro9900 do
      begin
         if Bloco_0.Registro0010Count > 0 then
         begin
            with New do
            begin
               REG_BLC := '0010';
               QTD_REG_BLC := Bloco_0.Registro0010Count;
            end;
         end;
         if Bloco_0.Registro0020Count > 0 then
         begin
            with New do
            begin
               REG_BLC := '0020';
               QTD_REG_BLC := Bloco_0.Registro0020Count;
            end;
         end;
         if Bloco_0.Registro0030Count > 0 then
         begin
            with New do
            begin
               REG_BLC := '0030';
               QTD_REG_BLC := Bloco_0.Registro0030Count;
            end;
         end;
         if Bloco_0.Registro0035Count > 0 then
         begin
            with New do
            begin
               REG_BLC := '0035';
               QTD_REG_BLC := Bloco_0.Registro0035Count;
            end;
         end;
         if Bloco_0.Registro0930Count > 0 then
         begin
            with New do
            begin
               REG_BLC := '0930';
               QTD_REG_BLC := Bloco_0.Registro0930Count;
            end;
         end;
      end;
   end;
end;

procedure TACBrSPEDECF.WriteRegistro0990;
begin
   with Bloco_9.Registro9900.New do
   begin
      REG_BLC := '0990';
      QTD_REG_BLC := 1;
   end;
   Bloco_0.WriteRegistro0990;
end;

procedure TACBrSPEDECF.WriteRegistroC001;
begin
  //Bloco_C.WriteRegistroC001;
end;

procedure TACBrSPEDECF.WriteRegistroC990;
begin
  with Bloco_9.Registro9900 do
  begin
     with New do
     begin
        REG_BLC := 'C001';
        QTD_REG_BLC := 1;
     end;
  end;

  //if Bloco_C.RegistroC001.IND_MOV = imComDados then
  //begin
  //  with Bloco_9.Registro9900 do
  //  begin
  //     if Bloco_C.RegistroC100Count > 0 then
  //     begin
  //       with New do
  //       begin
  //         REG_BLC := 'C100';
  //         QTD_REG_BLC := Bloco_C.RegistroC100Count;
  //       end;
  //     end;
  //     if Bloco_C.RegistroC105Count > 0 then
  //     begin
  //       with New do
  //       begin
  //         REG_BLC := 'C105';
  //         QTD_REG_BLC := Bloco_C.RegistroC105Count;
  //       end;
  //     end;
  //     if Bloco_C.RegistroC110Count > 0 then
  //     begin
  //       with New do
  //       begin
  //         REG_BLC := 'C110';
  //         QTD_REG_BLC := Bloco_C.RegistroC110Count;
  //       end;
  //     end;
  //     if Bloco_C.RegistroC111Count > 0 then
  //     begin
  //       with New do
  //       begin
  //         REG_BLC := 'C111';
  //         QTD_REG_BLC := Bloco_C.RegistroC111Count;
  //       end;
  //     end;
  //     if Bloco_C.RegistroC112Count > 0 then
  //     begin
  //       with New do
  //       begin
  //         REG_BLC := 'C112';
  //         QTD_REG_BLC := Bloco_C.RegistroC112Count;
  //       end;
  //     end;
  //     if Bloco_C.RegistroC113Count > 0 then
  //     begin
  //       with New do
  //       begin
  //         REG_BLC := 'C113';
  //         QTD_REG_BLC := Bloco_C.RegistroC113Count;
  //       end;
  //     end;
  //     if Bloco_C.RegistroC114Count > 0 then
  //     begin
  //       with New do
  //       begin
  //         REG_BLC := 'C114';
  //         QTD_REG_BLC := Bloco_C.RegistroC114Count;
  //       end;
  //     end;
  //     if Bloco_C.RegistroC115Count > 0 then
  //     begin
  //       with New do
  //       begin
  //         REG_BLC := 'C115';
  //         QTD_REG_BLC := Bloco_C.RegistroC115Count;
  //       end;
  //     end;
  //     if Bloco_C.RegistroC116Count > 0 then
  //     begin
  //       with New do
  //       begin
  //         REG_BLC := 'C116';
  //         QTD_REG_BLC := Bloco_C.RegistroC116Count;
  //       end;
  //     end;
  //     if Bloco_C.RegistroC120Count > 0 then
  //     begin
  //       with New do
  //       begin
  //         REG_BLC := 'C120';
  //         QTD_REG_BLC := Bloco_C.RegistroC120Count;
  //       end;
  //     end;
  //     if Bloco_C.RegistroC130Count > 0 then
  //     begin
  //       with New do
  //       begin
  //         REG_BLC := 'C130';
  //         QTD_REG_BLC := Bloco_C.RegistroC130Count;
  //       end;
  //     end;
  //     if Bloco_C.RegistroC140Count > 0 then
  //     begin
  //       with New do
  //       begin
  //         REG_BLC := 'C140';
  //         QTD_REG_BLC := Bloco_C.RegistroC140Count;   {Márcio Lopes 30Nov2009}
  //       end;
  //     end;
  //     if Bloco_C.RegistroC141Count > 0 then
  //     begin
  //       with New do
  //       begin
  //         REG_BLC := 'C141';
  //         QTD_REG_BLC := Bloco_C.RegistroC141Count;  {Márcio Lopes 30Nov2009}
  //       end;
  //     end;
  //     if Bloco_C.RegistroC160Count > 0 then
  //     begin
  //       with New do
  //       begin
  //         REG_BLC := 'C160';
  //         QTD_REG_BLC := Bloco_C.RegistroC160Count;
  //       end;
  //     end;
  //     if Bloco_C.RegistroC165Count > 0 then
  //     begin
  //       with New do
  //       begin
  //         REG_BLC := 'C165';
  //         QTD_REG_BLC := Bloco_C.RegistroC165Count;
  //       end;
  //     end;
  //     if Bloco_C.RegistroC170Count > 0 then
  //     begin
  //       with New do
  //       begin
  //         REG_BLC := 'C170';
  //         QTD_REG_BLC := Bloco_C.RegistroC170Count;
  //       end;
  //     end;
  //     if Bloco_C.RegistroC171Count > 0 then
  //     begin
  //       with New do
  //       begin
  //         REG_BLC := 'C171';
  //         QTD_REG_BLC := Bloco_C.RegistroC171Count;
  //       end;
  //     end;
  //     if Bloco_C.RegistroC172Count > 0 then
  //     begin
  //       with New do
  //       begin
  //         REG_BLC := 'C172';
  //         QTD_REG_BLC := Bloco_C.RegistroC172Count;
  //       end;
  //     end;
  //     if Bloco_C.RegistroC173Count > 0 then
  //     begin
  //       with New do
  //       begin
  //         REG_BLC := 'C173';
  //         QTD_REG_BLC := Bloco_C.RegistroC173Count;
  //       end;
  //     end;
  //     if Bloco_C.RegistroC174Count > 0 then
  //     begin
  //       with New do
  //       begin
  //         REG_BLC := 'C174';
  //         QTD_REG_BLC := Bloco_C.RegistroC174Count;
  //       end;
  //     end;
  //     if Bloco_C.RegistroC175Count > 0 then
  //     begin
  //       with New do
  //       begin
  //         REG_BLC := 'C175';
  //         QTD_REG_BLC := Bloco_C.RegistroC175Count;
  //       end;
  //     end;
  //     if Bloco_C.RegistroC176Count > 0 then
  //     begin
  //       with New do
  //       begin
  //         REG_BLC := 'C176';
  //         QTD_REG_BLC := Bloco_C.RegistroC176Count;
  //       end;
  //     end;
  //     if Bloco_C.RegistroC177Count > 0 then
  //     begin
  //       with New do
  //       begin
  //         REG_BLC := 'C177';
  //         QTD_REG_BLC := Bloco_C.RegistroC177Count;
  //       end;
  //     end;
  //     if Bloco_C.RegistroC178Count > 0 then
  //     begin
  //       with New do
  //       begin
  //         REG_BLC := 'C178';
  //         QTD_REG_BLC := Bloco_C.RegistroC178Count;
  //       end;
  //     end;
  //     if Bloco_C.RegistroC179Count > 0 then
  //     begin
  //       with New do
  //       begin
  //         REG_BLC := 'C179';
  //         QTD_REG_BLC := Bloco_C.RegistroC179Count;
  //       end;
  //     end;
  //     if Bloco_C.RegistroC190Count > 0 then
  //     begin
  //       with New do
  //       begin
  //         REG_BLC := 'C190';
  //         QTD_REG_BLC := Bloco_C.RegistroC190Count;
  //       end;
  //     end;
  //     if Bloco_C.RegistroC195Count > 0 then
  //     begin
  //       with New do
  //       begin
  //         REG_BLC := 'C195';
  //         QTD_REG_BLC := Bloco_C.RegistroC195Count;
  //       end;
  //     end;
  //     if Bloco_C.RegistroC197Count > 0 then
  //     begin
  //       with New do
  //       begin
  //         REG_BLC := 'C197';
  //         QTD_REG_BLC := Bloco_C.RegistroC197Count;
  //       end;
  //     end;
  //     if Bloco_C.RegistroC300Count > 0 then
  //     begin
  //       with New do
  //       begin
  //         REG_BLC := 'C300';
  //         QTD_REG_BLC := Bloco_C.RegistroC300Count;
  //       end;
  //     end;
  //     if Bloco_C.RegistroC310Count > 0 then
  //     begin
  //       with New do
  //       begin
  //         REG_BLC := 'C310';
  //         QTD_REG_BLC := Bloco_C.RegistroC310Count;
  //       end;
  //     end;
  //     if Bloco_C.RegistroC320Count > 0 then
  //     begin
  //       with New do
  //       begin
  //         REG_BLC := 'C320';
  //         QTD_REG_BLC := Bloco_C.RegistroC320Count;
  //       end;
  //     end;
  //     if Bloco_C.RegistroC321Count > 0 then
  //     begin
  //       with New do
  //       begin
  //         REG_BLC := 'C321';
  //         QTD_REG_BLC := Bloco_C.RegistroC321Count;
  //       end;
  //     end;
  //     if Bloco_C.RegistroC350Count > 0 then
  //     begin
  //       with New do
  //       begin
  //         REG_BLC := 'C350';
  //         QTD_REG_BLC := Bloco_C.RegistroC350Count;
  //       end;
  //     end;
  //     if Bloco_C.RegistroC370Count > 0 then
  //     begin
  //       with New do
  //       begin
  //         REG_BLC := 'C370';
  //         QTD_REG_BLC := Bloco_C.RegistroC370Count;
  //       end;
  //     end;
  //     if Bloco_C.RegistroC390Count > 0 then
  //     begin
  //       with New do
  //       begin
  //         REG_BLC := 'C390';
  //         QTD_REG_BLC := Bloco_C.RegistroC390Count;
  //       end;
  //     end;
  //     if Bloco_C.RegistroC400Count > 0 then
  //     begin
  //       with New do
  //       begin
  //         REG_BLC := 'C400';
  //         QTD_REG_BLC := Bloco_C.RegistroC400Count;
  //       end;
  //     end;
  //     if Bloco_C.RegistroC405Count > 0 then
  //     begin
  //       with New do
  //       begin
  //         REG_BLC := 'C405';
  //         QTD_REG_BLC := Bloco_C.RegistroC405Count;
  //       end;
  //     end;
  //     if Bloco_C.RegistroC410Count > 0 then
  //     begin
  //       with New do
  //       begin
  //         REG_BLC := 'C410';
  //         QTD_REG_BLC := Bloco_C.RegistroC410Count;
  //       end;
  //     end;
  //     if Bloco_C.RegistroC420Count > 0 then
  //     begin
  //       with New do
  //       begin
  //         REG_BLC := 'C420';
  //         QTD_REG_BLC := Bloco_C.RegistroC420Count;
  //       end;
  //     end;
  //     if Bloco_C.RegistroC425Count > 0 then
  //     begin
  //       with New do
  //       begin
  //         REG_BLC := 'C425';
  //         QTD_REG_BLC := Bloco_C.RegistroC425Count;
  //       end;
  //     end;
  //     if Bloco_C.RegistroC460Count > 0 then
  //     begin
  //       with New do
  //       begin
  //         REG_BLC := 'C460';
  //         QTD_REG_BLC := Bloco_C.RegistroC460Count;
  //       end;
  //     end;
  //     if Bloco_C.RegistroC470Count > 0 then
  //     begin
  //       with New do
  //       begin
  //         REG_BLC := 'C470';
  //         QTD_REG_BLC := Bloco_C.RegistroC470Count;
  //       end;
  //     end;
  //     if Bloco_C.RegistroC490Count > 0 then
  //     begin
  //       with New do
  //       begin
  //         REG_BLC := 'C490';
  //         QTD_REG_BLC := Bloco_C.RegistroC490Count;
  //       end;
  //     end;
  //     if Bloco_C.RegistroC495Count > 0 then
  //     begin
  //       with New do
  //       begin
  //         REG_BLC := 'C495';
  //         QTD_REG_BLC := Bloco_C.RegistroC495Count;
  //       end;
  //     end;
  //     if Bloco_C.RegistroC500Count > 0 then
  //     begin
  //       with New do
  //       begin
  //         REG_BLC := 'C500';
  //         QTD_REG_BLC := Bloco_C.RegistroC500Count;
  //       end;
  //     end;
  //     if Bloco_C.RegistroC510Count > 0 then
  //     begin
  //       with New do
  //       begin
  //         REG_BLC := 'C510';
  //         QTD_REG_BLC := Bloco_C.RegistroC510Count;
  //       end;
  //     end;
  //     if Bloco_C.RegistroC590Count > 0 then
  //     begin
  //       with New do
  //       begin
  //         REG_BLC := 'C590';
  //         QTD_REG_BLC := Bloco_C.RegistroC590Count;
  //       end;
  //     end;
  //     if Bloco_C.RegistroC600Count > 0 then
  //     begin
  //       with New do
  //       begin
  //         REG_BLC := 'C600';
  //         QTD_REG_BLC := Bloco_C.RegistroC600Count;
  //       end;
  //     end;
  //     if Bloco_C.RegistroC601Count > 0 then
  //     begin
  //       with New do
  //       begin
  //         REG_BLC := 'C601';
  //         QTD_REG_BLC := Bloco_C.RegistroC601Count;
  //       end;
  //     end;
  //     if Bloco_C.RegistroC610Count > 0 then
  //     begin
  //       with New do
  //       begin
  //         REG_BLC := 'C610';
  //         QTD_REG_BLC := Bloco_C.RegistroC610Count;
  //       end;
  //     end;
  //     if Bloco_C.RegistroC690Count > 0 then
  //     begin
  //       with New do
  //       begin
  //         REG_BLC := 'C690';
  //         QTD_REG_BLC := Bloco_C.RegistroC690Count;
  //       end;
  //     end;
  //     if Bloco_C.RegistroC700Count > 0 then
  //     begin
  //       with New do
  //       begin
  //         REG_BLC := 'C700';
  //         QTD_REG_BLC := Bloco_C.RegistroC700Count;
  //       end;
  //     end;
  //     if Bloco_C.RegistroC790Count > 0 then
  //     begin
  //       with New do
  //       begin
  //         REG_BLC := 'C790';
  //         QTD_REG_BLC := Bloco_C.RegistroC790Count;
  //       end;
  //     end;
  //     if Bloco_C.RegistroC791Count > 0 then
  //     begin
  //       with New do
  //       begin
  //         REG_BLC := 'C791';
  //         QTD_REG_BLC := Bloco_C.RegistroC791Count;
  //       end;
  //     end;
  //
  //     if Bloco_C.RegistroC800Count > 0 then
  //     begin
  //       with New do
  //       begin
  //         REG_BLC := 'C800';
  //         QTD_REG_BLC := Bloco_C.RegistroC800Count;
  //       end;
  //     end;
  //     if Bloco_C.RegistroC850Count > 0 then
  //     begin
  //       with New do
  //       begin
  //         REG_BLC := 'C850';
  //         QTD_REG_BLC := Bloco_C.RegistroC850Count;
  //       end;
  //     end;
  //
  //     if Bloco_C.RegistroC860Count > 0 then
  //     begin
  //       with New do
  //       begin
  //         REG_BLC := 'C860';
  //         QTD_REG_BLC := Bloco_C.RegistroC860Count;
  //       end;
  //     end;
  //     if Bloco_C.RegistroC890Count > 0 then
  //     begin
  //       with New do
  //       begin
  //         REG_BLC := 'C890';
  //         QTD_REG_BLC := Bloco_C.RegistroC890Count;
  //       end;
  //     end;
  //
  //  end;
  //end;

  with Bloco_9.Registro9900 do
  begin
     with New do
     begin
       REG_BLC := 'C990';
       QTD_REG_BLC := 1;
     end;
  end;

  //Bloco_C.WriteRegistroC990;
end;


procedure TACBrSPEDECF.WriteRegistroE001;
begin
   //Bloco_E.WriteRegistroE001;
   //
   with Bloco_9.Registro9900 do
   begin
      with New do
      begin
         REG_BLC := 'E001';
         QTD_REG_BLC := 1;
      end;
   end;
   //if Bloco_E.RegistroE001.IND_MOV = imComDados then
   //begin
   //   with Bloco_9.Registro9900 do
   //   begin
   //      with New do
   //      begin
   //         REG_BLC := 'E100';
   //         QTD_REG_BLC := Bloco_E.RegistroE100Count;
   //      end;
   //      with New do
   //      begin
   //         REG_BLC := 'E110';
   //         QTD_REG_BLC := Bloco_E.RegistroE110Count;
   //      end;
   //      if Bloco_E.RegistroE111Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := 'E111';
   //            QTD_REG_BLC := Bloco_E.RegistroE111Count;
   //         end;
   //      end;
   //      if Bloco_E.RegistroE112Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := 'E112';
   //            QTD_REG_BLC := Bloco_E.RegistroE112Count;
   //         end;
   //      end;
   //      if Bloco_E.RegistroE113Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := 'E113';
   //            QTD_REG_BLC := Bloco_E.RegistroE113Count;
   //         end;
   //      end;
   //      if Bloco_E.RegistroE115Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := 'E115';
   //            QTD_REG_BLC := Bloco_E.RegistroE115Count;
   //         end;
   //      end;
   //      if Bloco_E.RegistroE116Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := 'E116';
   //            QTD_REG_BLC := Bloco_E.RegistroE116Count;
   //         end;
   //      end;
   //      if Bloco_E.RegistroE200Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := 'E200';
   //            QTD_REG_BLC := Bloco_E.RegistroE200Count;
   //         end;
   //      end;
   //      if Bloco_E.RegistroE210Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := 'E210';
   //            QTD_REG_BLC := Bloco_E.RegistroE210Count;
   //         end;
   //      end;
   //      if Bloco_E.RegistroE220Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := 'E220';
   //            QTD_REG_BLC := Bloco_E.RegistroE220Count;
   //         end;
   //      end;
   //      if Bloco_E.RegistroE230Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := 'E230';
   //            QTD_REG_BLC := Bloco_E.RegistroE230Count;
   //         end;
   //      end;
   //      if Bloco_E.RegistroE240Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := 'E240';
   //            QTD_REG_BLC := Bloco_E.RegistroE240Count;
   //         end;
   //      end;
   //      if Bloco_E.RegistroE250Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := 'E250';
   //            QTD_REG_BLC := Bloco_E.RegistroE250Count;
   //         end;
   //      end;
   //      if Bloco_E.RegistroE500Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := 'E500';
   //            QTD_REG_BLC := Bloco_E.RegistroE500Count;
   //         end;
   //      end;
   //      if Bloco_E.RegistroE510Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := 'E510';
   //            QTD_REG_BLC := Bloco_E.RegistroE510Count;
   //         end;
   //      end;
   //      if Bloco_E.RegistroE520Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := 'E520';
   //            QTD_REG_BLC := Bloco_E.RegistroE520Count;
   //         end;
   //      end;
   //      if Bloco_E.RegistroE530Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := 'E530';
   //            QTD_REG_BLC := Bloco_E.RegistroE530Count;
   //         end;
   //      end;
   //   end;
   //end;
end;


procedure TACBrSPEDECF.WriteRegistroE990;
begin
  with Bloco_9.Registro9900.New do
  begin
    REG_BLC := 'E990';
    QTD_REG_BLC := 1;
  end;
  //Bloco_E.WriteRegistroE990;
end;

procedure TACBrSPEDECF.WriteRegistroJ001;
begin
  //Bloco_J.WriteRegistroJ001;
   //
   with Bloco_9.Registro9900 do
   begin
      with New do
      begin
         REG_BLC := 'J001';
         QTD_REG_BLC := 1;
      end;
   end;
   //if Bloco_J.RegistroJ001.IND_MOV = imComDados then
   //begin
   //   with Bloco_9.Registro9900 do
   //   begin
   //      if Bloco_K.RegistroK100Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := 'K100';
   //            QTD_REG_BLC := Bloco_K.RegistroK100Count;
   //         end;
   //      end;
   //      if Bloco_K.RegistroK200Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := 'K200';
   //            QTD_REG_BLC := Bloco_K.RegistroK200Count;
   //         end;
   //      end;
   //      if Bloco_K.RegistroK220Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := 'K220';
   //            QTD_REG_BLC := Bloco_K.RegistroK220Count;
   //         end;
   //      end;
   //      if Bloco_K.RegistroK230Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := 'K230';
   //            QTD_REG_BLC := Bloco_K.RegistroK230Count;
   //         end;
   //      end;
   //      if Bloco_K.RegistroK235Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := 'K235';
   //            QTD_REG_BLC := Bloco_K.RegistroK235Count;
   //         end;
   //      end;
   //      if Bloco_K.RegistroK250Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := 'K250';
   //            QTD_REG_BLC := Bloco_K.RegistroK250Count;
   //         end;
   //      end;
   //      if Bloco_K.RegistroK255Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := 'K255';
   //            QTD_REG_BLC := Bloco_K.RegistroK255Count;
   //         end;
   //      end;
   //   end;
   //end;
end;

procedure TACBrSPEDECF.WriteRegistroJ990;
begin
  with Bloco_9.Registro9900.New do
  begin
    REG_BLC := 'J990';
    QTD_REG_BLC := 1;
  end;
  //Bloco_J.WriteRegistroJ990;
end;

procedure TACBrSPEDECF.WriteRegistroK001;
begin
  //Bloco_K.WriteRegistroK001;
   //
   with Bloco_9.Registro9900 do
   begin
      with New do
      begin
         REG_BLC := 'K001';
         QTD_REG_BLC := 1;
      end;
   end;
   //if Bloco_K.RegistroK001.IND_MOV = imComDados then
   //begin
   //   with Bloco_9.Registro9900 do
   //   begin
   //      if Bloco_K.RegistroK100Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := 'K100';
   //            QTD_REG_BLC := Bloco_K.RegistroK100Count;
   //         end;
   //      end;
   //      if Bloco_K.RegistroK200Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := 'K200';
   //            QTD_REG_BLC := Bloco_K.RegistroK200Count;
   //         end;
   //      end;
   //      if Bloco_K.RegistroK220Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := 'K220';
   //            QTD_REG_BLC := Bloco_K.RegistroK220Count;
   //         end;
   //      end;
   //      if Bloco_K.RegistroK230Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := 'K230';
   //            QTD_REG_BLC := Bloco_K.RegistroK230Count;
   //         end;
   //      end;
   //      if Bloco_K.RegistroK235Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := 'K235';
   //            QTD_REG_BLC := Bloco_K.RegistroK235Count;
   //         end;
   //      end;
   //      if Bloco_K.RegistroK250Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := 'K250';
   //            QTD_REG_BLC := Bloco_K.RegistroK250Count;
   //         end;
   //      end;
   //      if Bloco_K.RegistroK255Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := 'K255';
   //            QTD_REG_BLC := Bloco_K.RegistroK255Count;
   //         end;
   //      end;
   //   end;
   //end;
end;

procedure TACBrSPEDECF.WriteRegistroK990;
begin
  with Bloco_9.Registro9900.New do
  begin
    REG_BLC := 'K990';
    QTD_REG_BLC := 1;
  end;
  //Bloco_K.WriteRegistroK990;
end;

procedure TACBrSPEDECF.WriteRegistroL001;
begin
   //Bloco_L.WriteRegistroL001;
   //
   with Bloco_9.Registro9900 do
   begin
      with New do
      begin
         REG_BLC := 'L001';
         QTD_REG_BLC := 1;
      end;
   end;
   //if Bloco_L.RegistroH001.IND_MOV = imComDados then
   //begin
   //   with Bloco_9.Registro9900 do
   //   begin
   //      if Bloco_H.RegistroH005Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := 'H005';
   //            QTD_REG_BLC := Bloco_H.RegistroH005Count;
   //         end;
   //      end;
   //      if Bloco_H.RegistroH010Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := 'H010';
   //            QTD_REG_BLC := Bloco_H.RegistroH010Count;
   //         end;
   //      end;
   //      if Bloco_H.RegistroH020Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := 'H020';
   //            QTD_REG_BLC := Bloco_H.RegistroH020Count;
   //         end;
   //      end;
   //   end;
   //end;
end;

procedure TACBrSPEDECF.WriteRegistroL990;
begin
  with Bloco_9.Registro9900.New do
  begin
    REG_BLC := 'H990';
    QTD_REG_BLC := 1;
  end;
  //Bloco_H.WriteRegistroH990;
end;

procedure TACBrSPEDECF.WriteRegistroM001;
begin
   //Bloco_M.WriteRegistroM001;
   //
   with Bloco_9.Registro9900 do
   begin
      with New do
      begin
         REG_BLC := 'M001';
         QTD_REG_BLC := 1;
      end;
   end;
   //if Bloco_M.RegistroG001.IND_MOV = imComDados then
   //begin
   //   with Bloco_9.Registro9900 do
   //   begin
   //      if Bloco_G.RegistroG110Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := 'G110';
   //            QTD_REG_BLC := Bloco_G.RegistroG110Count;
   //         end;
   //      end;
   //      if Bloco_G.RegistroG125Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := 'G125';
   //            QTD_REG_BLC := Bloco_G.RegistroG125Count;
   //         end;
   //      end;
   //      if Bloco_G.RegistroG126Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := 'G126';
   //            QTD_REG_BLC := Bloco_G.RegistroG126Count;
   //         end;
   //      end;
   //      if Bloco_G.RegistroG130Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := 'G130';
   //            QTD_REG_BLC := Bloco_G.RegistroG130Count;
   //         end;
   //      end;
   //      if Bloco_G.RegistroG140Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := 'G140';
   //            QTD_REG_BLC := Bloco_G.RegistroG140Count;
   //         end;
   //      end;
   //   end;
   //end;
end;

procedure TACBrSPEDECF.WriteRegistroM990;
begin
  with Bloco_9.Registro9900.New do
  begin
    REG_BLC := 'M990';
    QTD_REG_BLC := 1;
  end;
  //Bloco_M.WriteRegistroM990;
end;

procedure TACBrSPEDECF.WriteRegistroN001;
begin
   //Bloco_N.WriteRegistroN001;
   //
   with Bloco_9.Registro9900 do
   begin
      with New do
      begin
         REG_BLC := 'N001';
         QTD_REG_BLC := 1;
      end;
   end;
   //if Bloco_N.RegistroD001.IND_MOV = imComDados then
   //begin
   //   with Bloco_9.Registro9900 do
   //   begin
   //      if Bloco_D.RegistroD100Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := 'D100';
   //            QTD_REG_BLC := Bloco_D.RegistroD100Count;
   //         end;
   //      end;
   //      if Bloco_D.RegistroD110Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := 'D110';
   //            QTD_REG_BLC := Bloco_D.RegistroD110Count;
   //         end;
   //      end;
   //      if Bloco_D.RegistroD120Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := 'D120';
   //            QTD_REG_BLC := Bloco_D.RegistroD120Count;
   //         end;
   //      end;
   //      if Bloco_D.RegistroD130Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := 'D130';
   //            QTD_REG_BLC := Bloco_D.RegistroD130Count;
   //         end;
   //      end;
   //      if Bloco_D.RegistroD140Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := 'D140';
   //            QTD_REG_BLC := Bloco_D.RegistroD140Count;
   //         end;
   //      end;
   //      if Bloco_D.RegistroD150Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := 'D150';
   //            QTD_REG_BLC := Bloco_D.RegistroD150Count;
   //         end;
   //      end;
   //      if Bloco_D.RegistroD160Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := 'D160';
   //            QTD_REG_BLC := Bloco_D.RegistroD160Count;
   //         end;
   //      end;
   //      if Bloco_D.RegistroD161Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := 'D161';
   //            QTD_REG_BLC := Bloco_D.RegistroD161Count;
   //         end;
   //      end;
   //      if Bloco_D.RegistroD162Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := 'D162';
   //            QTD_REG_BLC := Bloco_D.RegistroD162Count;
   //         end;
   //      end;
   //      if Bloco_D.RegistroD170Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := 'D170';
   //            QTD_REG_BLC := Bloco_D.RegistroD170Count;
   //         end;
   //      end;
   //      if Bloco_D.RegistroD180Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := 'D180';
   //            QTD_REG_BLC := Bloco_D.RegistroD180Count;
   //         end;
   //      end;
   //      if Bloco_D.RegistroD190Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := 'D190';
   //            QTD_REG_BLC := Bloco_D.RegistroD190Count;
   //         end;
   //      end;
   //      if Bloco_D.RegistroD195Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := 'D195';
   //            QTD_REG_BLC := Bloco_D.RegistroD195Count;
   //         end;
   //      end;
   //      if Bloco_D.RegistroD197Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := 'D197';
   //            QTD_REG_BLC := Bloco_D.RegistroD197Count;
   //         end;
   //      end;
		 //
   //      if Bloco_D.RegistroD300Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := 'D300';
   //            QTD_REG_BLC := Bloco_D.RegistroD300Count;
   //         end;
   //      end;
   //      if Bloco_D.RegistroD301Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := 'D301';
   //            QTD_REG_BLC := Bloco_D.RegistroD301Count;
   //         end;
   //      end;
   //      if Bloco_D.RegistroD310Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := 'D310';
   //            QTD_REG_BLC := Bloco_D.RegistroD310Count;
   //         end;
   //      end;
   //      if Bloco_D.RegistroD350Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := 'D350';
   //            QTD_REG_BLC := Bloco_D.RegistroD350Count;
   //         end;
   //      end;
   //      if Bloco_D.RegistroD355Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := 'D355';
   //            QTD_REG_BLC := Bloco_D.RegistroD355Count;
   //         end;
   //      end;
   //      if Bloco_D.RegistroD360Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := 'D360';
   //            QTD_REG_BLC := Bloco_D.RegistroD360Count;
   //         end;
   //      end;
   //      if Bloco_D.RegistroD365Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := 'D365';
   //            QTD_REG_BLC := Bloco_D.RegistroD365Count;
   //         end;
   //      end;
   //      if Bloco_D.RegistroD370Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := 'D370';
   //            QTD_REG_BLC := Bloco_D.RegistroD370Count;
   //         end;
   //      end;
   //      if Bloco_D.RegistroD390Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := 'D390';
   //            QTD_REG_BLC := Bloco_D.RegistroD390Count;
   //         end;
   //      end;
   //      if Bloco_D.RegistroD400Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := 'D400';
   //            QTD_REG_BLC := Bloco_D.RegistroD400Count;
   //         end;
   //      end;
   //      if Bloco_D.RegistroD410Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := 'D410';
   //            QTD_REG_BLC := Bloco_D.RegistroD410Count;
   //         end;
   //      end;
   //      if Bloco_D.RegistroD411Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := 'D411';
   //            QTD_REG_BLC := Bloco_D.RegistroD411Count;
   //         end;
   //      end;
   //      if Bloco_D.RegistroD420Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := 'D420';
   //            QTD_REG_BLC := Bloco_D.RegistroD420Count;
   //         end;
   //      end;
   //      if Bloco_D.RegistroD500Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := 'D500';
   //            QTD_REG_BLC := Bloco_D.RegistroD500Count;
   //         end;
   //      end;
   //      if Bloco_D.RegistroD510Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := 'D510';
   //            QTD_REG_BLC := Bloco_D.RegistroD510Count;
   //         end;
   //      end;
   //      if Bloco_D.RegistroD530Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := 'D530';
   //            QTD_REG_BLC := Bloco_D.RegistroD530Count;
   //         end;
   //      end;
   //      if Bloco_D.RegistroD590Count > 0 then
   //      begin
   //         with New do
   //         begin
   //           REG_BLC := 'D590';
   //           QTD_REG_BLC := Bloco_D.RegistroD590Count;
   //         end;
   //      end;
   //      if Bloco_D.RegistroD600Count > 0 then
   //      begin
   //         with New do
   //         begin
   //           REG_BLC := 'D600';
   //           QTD_REG_BLC := Bloco_D.RegistroD600Count;
   //         end;
   //      end;
   //      if Bloco_D.RegistroD610Count > 0 then
   //      begin
   //         with New do
   //         begin
   //           REG_BLC := 'D610';
   //           QTD_REG_BLC := Bloco_D.RegistroD610Count;
   //         end;
   //      end;
   //      if Bloco_D.RegistroD690Count > 0 then
   //      begin
   //         with New do
   //         begin
   //           REG_BLC := 'D690';
   //           QTD_REG_BLC := Bloco_D.RegistroD690Count;
   //         end;
   //      end;
   //      if Bloco_D.RegistroD695Count > 0 then
   //      begin
   //         with New do
   //         begin
   //           REG_BLC := 'D695';
   //           QTD_REG_BLC := Bloco_D.RegistroD695Count;
   //         end;
   //      end;
   //      if Bloco_D.RegistroD696Count > 0 then
   //      begin
   //         with New do
   //         begin
   //           REG_BLC := 'D696';
   //           QTD_REG_BLC := Bloco_D.RegistroD696Count;
   //         end;
   //      end;
   //      if Bloco_D.RegistroD697Count > 0 then
   //      begin
   //         with New do
   //         begin
   //           REG_BLC := 'D697';
   //           QTD_REG_BLC := Bloco_D.RegistroD697Count;
   //         end;
   //      end;
   //   end;
   //end;
end;

procedure TACBrSPEDECF.WriteRegistroN990;
begin
  with Bloco_9.Registro9900.New do
  begin
    REG_BLC := 'N990';
    QTD_REG_BLC := 1;
  end;
  //Bloco_N.WriteRegistroN990;
end;

procedure TACBrSPEDECF.WriteRegistroP001;
begin
   //Bloco_P.WriteRegistroP001;
   //
   with Bloco_9.Registro9900 do
   begin
      with New do
      begin
         REG_BLC := 'P001';
         QTD_REG_BLC := 1;
      end;
   end;
   //if Bloco_P.RegistroP001.IND_MOV = imComDados then
   //begin
   //   with Bloco_9.Registro9900 do
   //   begin
   //      with New do
   //      begin
   //         REG_BLC := 'E100';
   //         QTD_REG_BLC := Bloco_E.RegistroE100Count;
   //      end;
   //      with New do
   //      begin
   //         REG_BLC := 'E110';
   //         QTD_REG_BLC := Bloco_E.RegistroE110Count;
   //      end;
   //      if Bloco_E.RegistroE111Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := 'E111';
   //            QTD_REG_BLC := Bloco_E.RegistroE111Count;
   //         end;
   //      end;
   //      if Bloco_E.RegistroE112Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := 'E112';
   //            QTD_REG_BLC := Bloco_E.RegistroE112Count;
   //         end;
   //      end;
   //      if Bloco_E.RegistroE113Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := 'E113';
   //            QTD_REG_BLC := Bloco_E.RegistroE113Count;
   //         end;
   //      end;
   //      if Bloco_E.RegistroE115Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := 'E115';
   //            QTD_REG_BLC := Bloco_E.RegistroE115Count;
   //         end;
   //      end;
   //      if Bloco_E.RegistroE116Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := 'E116';
   //            QTD_REG_BLC := Bloco_E.RegistroE116Count;
   //         end;
   //      end;
   //      if Bloco_E.RegistroE200Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := 'E200';
   //            QTD_REG_BLC := Bloco_E.RegistroE200Count;
   //         end;
   //      end;
   //      if Bloco_E.RegistroE210Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := 'E210';
   //            QTD_REG_BLC := Bloco_E.RegistroE210Count;
   //         end;
   //      end;
   //      if Bloco_E.RegistroE220Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := 'E220';
   //            QTD_REG_BLC := Bloco_E.RegistroE220Count;
   //         end;
   //      end;
   //      if Bloco_E.RegistroE230Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := 'E230';
   //            QTD_REG_BLC := Bloco_E.RegistroE230Count;
   //         end;
   //      end;
   //      if Bloco_E.RegistroE240Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := 'E240';
   //            QTD_REG_BLC := Bloco_E.RegistroE240Count;
   //         end;
   //      end;
   //      if Bloco_E.RegistroE250Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := 'E250';
   //            QTD_REG_BLC := Bloco_E.RegistroE250Count;
   //         end;
   //      end;
   //      if Bloco_E.RegistroE500Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := 'E500';
   //            QTD_REG_BLC := Bloco_E.RegistroE500Count;
   //         end;
   //      end;
   //      if Bloco_E.RegistroE510Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := 'E510';
   //            QTD_REG_BLC := Bloco_E.RegistroE510Count;
   //         end;
   //      end;
   //      if Bloco_E.RegistroE520Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := 'E520';
   //            QTD_REG_BLC := Bloco_E.RegistroE520Count;
   //         end;
   //      end;
   //      if Bloco_E.RegistroE530Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := 'E530';
   //            QTD_REG_BLC := Bloco_E.RegistroE530Count;
   //         end;
   //      end;
   //   end;
   //end;
end;

procedure TACBrSPEDECF.WriteRegistroP990;
begin
  with Bloco_9.Registro9900.New do
  begin
    REG_BLC := 'P990';
    QTD_REG_BLC := 1;
  end;
  //Bloco_P.WriteRegistroP990;
end;

procedure TACBrSPEDECF.WriteRegistroT001;
begin
   //Bloco_T.WriteRegistroT001;
   //
   with Bloco_9.Registro9900 do
   begin
      with New do
      begin
         REG_BLC := 'T001';
         QTD_REG_BLC := 1;
      end;
   end;
   //if Bloco_T.RegistroT001.IND_MOV = imComDados then
   //begin
   //   with Bloco_9.Registro9900 do
   //   begin
   //      with New do
   //      begin
   //         REG_BLC := 'E100';
   //         QTD_REG_BLC := Bloco_E.RegistroE100Count;
   //      end;
   //      with New do
   //      begin
   //         REG_BLC := 'E110';
   //         QTD_REG_BLC := Bloco_E.RegistroE110Count;
   //      end;
   //      if Bloco_E.RegistroE111Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := 'E111';
   //            QTD_REG_BLC := Bloco_E.RegistroE111Count;
   //         end;
   //      end;
   //      if Bloco_E.RegistroE112Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := 'E112';
   //            QTD_REG_BLC := Bloco_E.RegistroE112Count;
   //         end;
   //      end;
   //      if Bloco_E.RegistroE113Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := 'E113';
   //            QTD_REG_BLC := Bloco_E.RegistroE113Count;
   //         end;
   //      end;
   //      if Bloco_E.RegistroE115Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := 'E115';
   //            QTD_REG_BLC := Bloco_E.RegistroE115Count;
   //         end;
   //      end;
   //      if Bloco_E.RegistroE116Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := 'E116';
   //            QTD_REG_BLC := Bloco_E.RegistroE116Count;
   //         end;
   //      end;
   //      if Bloco_E.RegistroE200Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := 'E200';
   //            QTD_REG_BLC := Bloco_E.RegistroE200Count;
   //         end;
   //      end;
   //      if Bloco_E.RegistroE210Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := 'E210';
   //            QTD_REG_BLC := Bloco_E.RegistroE210Count;
   //         end;
   //      end;
   //      if Bloco_E.RegistroE220Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := 'E220';
   //            QTD_REG_BLC := Bloco_E.RegistroE220Count;
   //         end;
   //      end;
   //      if Bloco_E.RegistroE230Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := 'E230';
   //            QTD_REG_BLC := Bloco_E.RegistroE230Count;
   //         end;
   //      end;
   //      if Bloco_E.RegistroE240Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := 'E240';
   //            QTD_REG_BLC := Bloco_E.RegistroE240Count;
   //         end;
   //      end;
   //      if Bloco_E.RegistroE250Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := 'E250';
   //            QTD_REG_BLC := Bloco_E.RegistroE250Count;
   //         end;
   //      end;
   //      if Bloco_E.RegistroE500Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := 'E500';
   //            QTD_REG_BLC := Bloco_E.RegistroE500Count;
   //         end;
   //      end;
   //      if Bloco_E.RegistroE510Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := 'E510';
   //            QTD_REG_BLC := Bloco_E.RegistroE510Count;
   //         end;
   //      end;
   //      if Bloco_E.RegistroE520Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := 'E520';
   //            QTD_REG_BLC := Bloco_E.RegistroE520Count;
   //         end;
   //      end;
   //      if Bloco_E.RegistroE530Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := 'E530';
   //            QTD_REG_BLC := Bloco_E.RegistroE530Count;
   //         end;
   //      end;
   //   end;
   //end;
end;

procedure TACBrSPEDECF.WriteRegistroT990;
begin
  with Bloco_9.Registro9900.New do
  begin
    REG_BLC := 'T990';
    QTD_REG_BLC := 1;
  end;
  //Bloco_T.WriteRegistroT990;
end;

procedure TACBrSPEDECF.WriteRegistroU001;
begin
   //Bloco_U.WriteRegistroU001;
   //
   with Bloco_9.Registro9900 do
   begin
      with New do
      begin
         REG_BLC := 'U001';
         QTD_REG_BLC := 1;
      end;
   end;
   //if Bloco_U.RegistroU001.IND_MOV = imComDados then
   //begin
   //   with Bloco_9.Registro9900 do
   //   begin
   //      with New do
   //      begin
   //         REG_BLC := 'E100';
   //         QTD_REG_BLC := Bloco_E.RegistroE100Count;
   //      end;
   //      with New do
   //      begin
   //         REG_BLC := 'E110';
   //         QTD_REG_BLC := Bloco_E.RegistroE110Count;
   //      end;
   //      if Bloco_E.RegistroE111Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := 'E111';
   //            QTD_REG_BLC := Bloco_E.RegistroE111Count;
   //         end;
   //      end;
   //      if Bloco_E.RegistroE112Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := 'E112';
   //            QTD_REG_BLC := Bloco_E.RegistroE112Count;
   //         end;
   //      end;
   //      if Bloco_E.RegistroE113Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := 'E113';
   //            QTD_REG_BLC := Bloco_E.RegistroE113Count;
   //         end;
   //      end;
   //      if Bloco_E.RegistroE115Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := 'E115';
   //            QTD_REG_BLC := Bloco_E.RegistroE115Count;
   //         end;
   //      end;
   //      if Bloco_E.RegistroE116Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := 'E116';
   //            QTD_REG_BLC := Bloco_E.RegistroE116Count;
   //         end;
   //      end;
   //      if Bloco_E.RegistroE200Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := 'E200';
   //            QTD_REG_BLC := Bloco_E.RegistroE200Count;
   //         end;
   //      end;
   //      if Bloco_E.RegistroE210Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := 'E210';
   //            QTD_REG_BLC := Bloco_E.RegistroE210Count;
   //         end;
   //      end;
   //      if Bloco_E.RegistroE220Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := 'E220';
   //            QTD_REG_BLC := Bloco_E.RegistroE220Count;
   //         end;
   //      end;
   //      if Bloco_E.RegistroE230Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := 'E230';
   //            QTD_REG_BLC := Bloco_E.RegistroE230Count;
   //         end;
   //      end;
   //      if Bloco_E.RegistroE240Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := 'E240';
   //            QTD_REG_BLC := Bloco_E.RegistroE240Count;
   //         end;
   //      end;
   //      if Bloco_E.RegistroE250Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := 'E250';
   //            QTD_REG_BLC := Bloco_E.RegistroE250Count;
   //         end;
   //      end;
   //      if Bloco_E.RegistroE500Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := 'E500';
   //            QTD_REG_BLC := Bloco_E.RegistroE500Count;
   //         end;
   //      end;
   //      if Bloco_E.RegistroE510Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := 'E510';
   //            QTD_REG_BLC := Bloco_E.RegistroE510Count;
   //         end;
   //      end;
   //      if Bloco_E.RegistroE520Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := 'E520';
   //            QTD_REG_BLC := Bloco_E.RegistroE520Count;
   //         end;
   //      end;
   //      if Bloco_E.RegistroE530Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := 'E530';
   //            QTD_REG_BLC := Bloco_E.RegistroE530Count;
   //         end;
   //      end;
   //   end;
   //end;
end;

procedure TACBrSPEDECF.WriteRegistroU990;
begin
  with Bloco_9.Registro9900.New do
  begin
    REG_BLC := 'U990';
    QTD_REG_BLC := 1;
  end;
  //Bloco_U.WriteRegistroU990;
end;

procedure TACBrSPEDECF.WriteRegistroX001;
begin
   //Bloco_X.WriteRegistroX001;
   //
   with Bloco_9.Registro9900 do
   begin
      with New do
      begin
         REG_BLC := 'X001';
         QTD_REG_BLC := 1;
      end;
   end;
   //if Bloco_X.RegistroX001.IND_MOV = imComDados then
   //begin
   //   with Bloco_9.Registro9900 do
   //   begin
   //      with New do
   //      begin
   //         REG_BLC := 'E100';
   //         QTD_REG_BLC := Bloco_E.RegistroE100Count;
   //      end;
   //      with New do
   //      begin
   //         REG_BLC := 'E110';
   //         QTD_REG_BLC := Bloco_E.RegistroE110Count;
   //      end;
   //      if Bloco_E.RegistroE111Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := 'E111';
   //            QTD_REG_BLC := Bloco_E.RegistroE111Count;
   //         end;
   //      end;
   //      if Bloco_E.RegistroE112Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := 'E112';
   //            QTD_REG_BLC := Bloco_E.RegistroE112Count;
   //         end;
   //      end;
   //      if Bloco_E.RegistroE113Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := 'E113';
   //            QTD_REG_BLC := Bloco_E.RegistroE113Count;
   //         end;
   //      end;
   //      if Bloco_E.RegistroE115Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := 'E115';
   //            QTD_REG_BLC := Bloco_E.RegistroE115Count;
   //         end;
   //      end;
   //      if Bloco_E.RegistroE116Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := 'E116';
   //            QTD_REG_BLC := Bloco_E.RegistroE116Count;
   //         end;
   //      end;
   //      if Bloco_E.RegistroE200Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := 'E200';
   //            QTD_REG_BLC := Bloco_E.RegistroE200Count;
   //         end;
   //      end;
   //      if Bloco_E.RegistroE210Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := 'E210';
   //            QTD_REG_BLC := Bloco_E.RegistroE210Count;
   //         end;
   //      end;
   //      if Bloco_E.RegistroE220Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := 'E220';
   //            QTD_REG_BLC := Bloco_E.RegistroE220Count;
   //         end;
   //      end;
   //      if Bloco_E.RegistroE230Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := 'E230';
   //            QTD_REG_BLC := Bloco_E.RegistroE230Count;
   //         end;
   //      end;
   //      if Bloco_E.RegistroE240Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := 'E240';
   //            QTD_REG_BLC := Bloco_E.RegistroE240Count;
   //         end;
   //      end;
   //      if Bloco_E.RegistroE250Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := 'E250';
   //            QTD_REG_BLC := Bloco_E.RegistroE250Count;
   //         end;
   //      end;
   //      if Bloco_E.RegistroE500Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := 'E500';
   //            QTD_REG_BLC := Bloco_E.RegistroE500Count;
   //         end;
   //      end;
   //      if Bloco_E.RegistroE510Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := 'E510';
   //            QTD_REG_BLC := Bloco_E.RegistroE510Count;
   //         end;
   //      end;
   //      if Bloco_E.RegistroE520Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := 'E520';
   //            QTD_REG_BLC := Bloco_E.RegistroE520Count;
   //         end;
   //      end;
   //      if Bloco_E.RegistroE530Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := 'E530';
   //            QTD_REG_BLC := Bloco_E.RegistroE530Count;
   //         end;
   //      end;
   //   end;
   //end;
end;

procedure TACBrSPEDECF.WriteRegistroX990;
begin
  with Bloco_9.Registro9900.New do
  begin
    REG_BLC := 'X990';
    QTD_REG_BLC := 1;
  end;
  //Bloco_X.WriteRegistroX990;
end;

procedure TACBrSPEDECF.WriteRegistroY001;
begin
   //Bloco_Y.WriteRegistroY001;
   //
   //with Bloco_9.Registro9900 do
   //begin
   //   with New do
   //   begin
   //      REG_BLC := '1001';
   //      QTD_REG_BLC := 1;
   //   end;
   //end;
   //if Bloco_1.Registro1001.IND_MOV = imComDados then
   //begin
   //   with Bloco_9.Registro9900 do
   //   begin
   //      if Bloco_1.Registro1010Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := '1010';
   //            QTD_REG_BLC := Bloco_1.Registro1010Count;
   //         end;
   //      end;
   //      if Bloco_1.Registro1100Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := '1100';
   //            QTD_REG_BLC := Bloco_1.Registro1100Count;
   //         end;
   //      end;
   //      if Bloco_1.Registro1105Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := '1105';
   //            QTD_REG_BLC := Bloco_1.Registro1105Count;
   //         end;
   //      end;
   //      if Bloco_1.Registro1110Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := '1110';
   //            QTD_REG_BLC := Bloco_1.Registro1110Count;
   //         end;
   //      end;
   //      if Bloco_1.Registro1200Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := '1200';
   //            QTD_REG_BLC := Bloco_1.Registro1200Count;
   //         end;
   //      end;
   //      if Bloco_1.Registro1210Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := '1210';
   //            QTD_REG_BLC := Bloco_1.Registro1210Count;
   //         end;
   //      end;
   //      if Bloco_1.Registro1300Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := '1300';
   //            QTD_REG_BLC := Bloco_1.Registro1300Count;
   //         end;
   //      end;
   //      if Bloco_1.Registro1310Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := '1310';
   //            QTD_REG_BLC := Bloco_1.Registro1310Count;
   //         end;
   //      end;
   //      if Bloco_1.Registro1320Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := '1320';
   //            QTD_REG_BLC := Bloco_1.Registro1320Count;
   //         end;
   //      end;
   //      if Bloco_1.Registro1350Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := '1350';
   //            QTD_REG_BLC := Bloco_1.Registro1350Count;
   //         end;
   //      end;
   //      if Bloco_1.Registro1360Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := '1360';
   //            QTD_REG_BLC := Bloco_1.Registro1360Count;
   //         end;
   //      end;
   //      if Bloco_1.Registro1370Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := '1370';
   //            QTD_REG_BLC := Bloco_1.Registro1370Count;
   //         end;
   //      end;
   //      if Bloco_1.Registro1390Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := '1390';
   //            QTD_REG_BLC := Bloco_1.Registro1390Count;
   //         end;
   //      end;
   //      if Bloco_1.Registro1391Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := '1391';
   //            QTD_REG_BLC := Bloco_1.Registro1391Count;
   //         end;
   //      end;
   //      if Bloco_1.Registro1400Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := '1400';
   //            QTD_REG_BLC := Bloco_1.Registro1400Count;
   //         end;
   //      end;
   //      if Bloco_1.Registro1500Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := '1500';
   //            QTD_REG_BLC := Bloco_1.Registro1500Count;
   //         end;
   //      end;
   //      if Bloco_1.Registro1510Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := '1510';
   //            QTD_REG_BLC := Bloco_1.Registro1510Count;
   //         end;
   //      end;
   //      if Bloco_1.Registro1600Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := '1600';
   //            QTD_REG_BLC := Bloco_1.Registro1600Count;
   //         end;
   //      end;
   //      if Bloco_1.Registro1700Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := '1700';
   //            QTD_REG_BLC := Bloco_1.Registro1700Count;
   //         end;
   //      end;
   //      if Bloco_1.Registro1710Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := '1710';
   //            QTD_REG_BLC := Bloco_1.Registro1710Count;
   //         end;
   //      end;
   //      if Bloco_1.Registro1800Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := '1800';
   //            QTD_REG_BLC := Bloco_1.Registro1800Count;
   //         end;
   //      end;
   //      if Bloco_1.Registro1900Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := '1900';
   //            QTD_REG_BLC := Bloco_1.Registro1900Count;
   //         end;
   //      end;
   //      if Bloco_1.Registro1910Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := '1910';
   //            QTD_REG_BLC := Bloco_1.Registro1910Count;
   //         end;
   //      end;
   //      if Bloco_1.Registro1920Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := '1920';
   //            QTD_REG_BLC := Bloco_1.Registro1920Count;
   //         end;
   //      end;
   //      if Bloco_1.Registro1921Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := '1921';
   //            QTD_REG_BLC := Bloco_1.Registro1921Count;
   //         end;
   //      end;
   //      if Bloco_1.Registro1922Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := '1922';
   //            QTD_REG_BLC := Bloco_1.Registro1922Count;
   //         end;
   //      end;
   //      if Bloco_1.Registro1923Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := '1923';
   //            QTD_REG_BLC := Bloco_1.Registro1923Count;
   //         end;
   //      end;
   //      if Bloco_1.Registro1925Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := '1925';
   //            QTD_REG_BLC := Bloco_1.Registro1925Count;
   //         end;
   //      end;
   //      if Bloco_1.Registro1926Count > 0 then
   //      begin
   //         with New do
   //         begin
   //            REG_BLC := '1926';
   //            QTD_REG_BLC := Bloco_1.Registro1926Count;
   //         end;
   //      end;
   //   end;
   //end;
end;

procedure TACBrSPEDECF.WriteRegistroY990;
begin
   with Bloco_9.Registro9900.New do
   begin
      REG_BLC := 'Y990';
      QTD_REG_BLC := 1;
   end;
   //Bloco_Y.WriteRegistroY990;
end;

procedure TACBrSPEDECF.WriteRegistro9001;
begin
  with Bloco_9.Registro9900.New do
  begin
    REG_BLC := '9001';
    QTD_REG_BLC := 1;
  end;
  Bloco_9.WriteRegistro9001;
end;

procedure TACBrSPEDECF.WriteRegistro9900;
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

procedure TACBrSPEDECF.WriteRegistro9990;
begin
  Bloco_9.WriteRegistro9990;
end;

procedure TACBrSPEDECF.WriteRegistro9999;
begin
  //Bloco_9.Registro9999.QTD_LIN := Bloco_9.Registro9999.QTD_LIN + Bloco_0.Registro0990.QTD_LIN_0 +
  //                                                               Bloco_1.Registro1990.QTD_LIN_1 +
  //                                                               Bloco_C.RegistroC990.QTD_LIN_C +
  //                                                               Bloco_D.RegistroD990.QTD_LIN_D +
  //                                                               Bloco_E.RegistroE990.QTD_LIN_E +
  //                                                               ifThen(Bloco_G.DT_INI >= EncodeDate(2011,01,01), Bloco_G.RegistroG990.QTD_LIN_G, 0) +
  //                                                               ifthen(Bloco_K.DT_INI >= EncodeDate(2015,01,01), Bloco_K.RegistroK990.QTD_LIN_K, 0) +
  //                                                               Bloco_H.RegistroH990.QTD_LIN_H +
  //                                                               Bloco_9.Registro9990.QTD_LIN_9;
  Bloco_9.WriteRegistro9999;
end;

{$IFNDEF Framework}
{$IFDEF FPC}
initialization
   {$I ACBrSpedECF.lrs}
{$ENDIF}
{$ENDIF}

end.

