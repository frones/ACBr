{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2010   Isaque Pinheiro                      }
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
|* 07/12/2010: Isaque Pinheiro
|*  - Criação e distribuição da Primeira Versao
*******************************************************************************}

unit ACBrSpedPisCofins;

interface

uses
  SysUtils, Math, Classes, ACBrBase,
  {$IFDEF FPC}
    LResources,
  {$ENDIF}
  DateUtils, ACBrSped, ACBrTXTClass, ACBrEPCBlocos,
  ACBrEPCBloco_0_Class, ACBrEPCBloco_1_Class, ACBrEPCBloco_9_Class,
  ACBrEPCBloco_A_Class, ACBrEPCBloco_C_Class, ACBrEPCBloco_D_Class,
  ACBrEPCBloco_F_Class, ACBrEPCBloco_M_Class, ACBrEPCBloco_P_Class,
  ACBrEPCBloco_C_Events, ACBrEPCBloco_I_Class;

const
  CACBrSpedPisCofins_Versao = '1.02';

type
  { TACBrSPEDPisCofins }
	{$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TACBrSPEDPisCofins = class(TACBrComponent)
  private
    FACBrTXT: TACBrTXTClass;
    FArquivo: String;
    FInicializado : boolean;
    FOnError: TErrorEvent;

    FEventsBloco_C: TEventsBloco_C;

    FDT_INI: TDateTime;           /// Data inicial das informações contidas no arquivo
    FDT_FIN: TDateTime;           /// Data final das informações contidas no arquivo

    FPath: String;            /// Path do arquivo a ser gerado
    FDelimitador: String;     /// Caracter delimitador de campos
    FTrimString: boolean;
    /// Retorna a string sem espaços em branco iniciais e finais
    FCurMascara: String;      /// Mascara para valores tipo currency

    FBloco_0: TBloco_0;
    FBloco_1: TBloco_1;
    FBloco_9: TBloco_9;
    FBloco_A: TBloco_A;
    FBloco_C: TBloco_C;
    FBloco_D: TBloco_D;
    FBloco_F: TBloco_F;
    FBloco_I: TBloco_I;
    FBloco_M: TBloco_M;
    FBloco_P: TBloco_P;

    function GetAbout: String;
    function GetConteudo: TStringList;
    function GetDelimitador: String;
    function GetLinhasBuffer: Integer;
    function GetTrimString: boolean;
    function GetCurMascara: String;
    function GetDT_FIN: TDateTime;
    function GetDT_INI: TDateTime;
    procedure InicializaBloco(Bloco: TACBrSPED);
    procedure SetArquivo(const AValue: String);
    procedure SetDelimitador(const Value: String);
    procedure SetLinhasBuffer(const AValue: Integer);
    procedure SetPath(const AValue: String);
    procedure SetTrimString(const Value: boolean);
    procedure SetCurMascara(const Value: String);
    procedure SetDT_FIN(const Value: TDateTime);
    procedure SetDT_INI(const Value: TDateTime);

    function GetOnError: TErrorEvent; /// Método do evento OnError
    procedure SetOnError(const Value: TErrorEvent); /// Método SetError

    procedure IniciaDados;
  protected
    /// BLOCO 0
    procedure WriteRegistro0000;
    procedure WriteRegistro0001;
    procedure WriteRegistro0990;
    /// BLOCO 1
    procedure WriteRegistro1001;
    procedure WriteRegistro1990;
    /// BLOCO A
    procedure WriteRegistroA001;
    procedure WriteRegistroA990;
    /// BLOCO C
    procedure WriteRegistroC001;
    procedure WriteRegistroC990;
    /// BLOCO D
    procedure WriteRegistroD001;
    procedure WriteRegistroD990;
    /// BLOCO F
    procedure WriteRegistroF001;
    procedure WriteRegistroF990;
    /// BLOCO I
    procedure WriteRegistroI001;
    procedure WriteRegistroI990;
    /// BLOCO M
    procedure WriteRegistroM001;
    procedure WriteRegistroM990;
    /// BLOCO P
    procedure WriteRegistroP001;
    procedure WriteRegistroP990;

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
    procedure LimpaRegistros;

    procedure WriteBloco_0;
    procedure WriteBloco_1;
    procedure WriteBloco_9;
    procedure WriteBloco_A( FechaBloco: Boolean );
    procedure WriteBloco_C( FechaBloco: Boolean );
    procedure WriteBloco_D;
    procedure WriteBloco_F;
    procedure WriteBloco_I;
    procedure WriteBloco_M;
    procedure WriteBloco_P;

    property Conteudo: TStringList read GetConteudo;

    property DT_INI: TDateTime read GetDT_INI write SetDT_INI;
    property DT_FIN: TDateTime read GetDT_FIN write SetDT_FIN;

    property Bloco_0: TBloco_0 read FBloco_0 write FBloco_0;
    property Bloco_1: TBloco_1 read FBloco_1 write FBloco_1;
    property Bloco_9: TBloco_9 read FBloco_9 write FBloco_9;
    property Bloco_A: TBloco_A read FBloco_A write FBloco_A;
    property Bloco_C: TBloco_C read FBloco_C write FBloco_C;
    property Bloco_D: TBloco_D read FBloco_D write FBloco_D;
    property Bloco_F: TBloco_F read FBloco_F write FBloco_F;
    property Bloco_I: TBloco_I read FBloco_I write FBloco_I;
    property Bloco_M: TBloco_M read FBloco_M write FBloco_M;
    property Bloco_P: TBloco_P read FBloco_P write FBloco_P;
  published
    property About: String read GetAbout stored False;
    property Path: String read FPath write SetPath;
    property Arquivo: String read FArquivo write SetArquivo;
    property LinhasBuffer : Integer read GetLinhasBuffer write SetLinhasBuffer
      default 1000 ;

    ///
    property Delimitador: String read GetDelimitador write SetDelimitador;
    property TrimString: boolean read GetTrimString write SetTrimString;
    property CurMascara: String read GetCurMascara write SetCurMascara;

    property OnError: TErrorEvent read GetOnError write SetOnError;

    // Eventos
    property EventsBloco_C: TEventsBloco_C read FEventsBloco_C;
  end;

procedure Register;

implementation

uses ACBrUtil;

{$IFNDEF FPC}
 {$R ACBr_SPEDPisCofins.dcr}
{$ENDIF}

procedure Register;
begin
  RegisterComponents('ACBrTXT', [TACBrSPEDPisCofins]);
end;

(* TACBrSPEDPisCofins *)

constructor TACBrSPEDPisCofins.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FACBrTXT := TACBrTXTClass.Create;
  FACBrTXT.LinhasBuffer := 1000 ;

  FInicializado := False;

  FBloco_0 := TBloco_0.Create;
  FBloco_A := TBloco_A.Create;
  FBloco_C := TBloco_C.Create;
  FBloco_D := TBloco_D.Create;
  FBloco_F := TBloco_F.Create;
  FBloco_I := TBloco_I.Create;
  FBloco_M := TBloco_M.Create;
  FBloco_P := TBloco_P.Create;
  FBloco_1 := TBloco_1.Create;
  FBloco_9 := TBloco_9.Create;

  // Define valores iniciais as propriedades no create e após limpar os registros.
  IniciaDados;

  /// Objeto passado por referência para que possamos usa-lo para fazer pesquisa
  /// em seus registros.
  /// Ex: Do Bloco_C registro C425, pesquisar o Bloco_0 registro 0200.
  FBloco_1.Bloco_0 := FBloco_0;
  FBloco_A.Bloco_0 := FBloco_0;
  FBloco_C.Bloco_0 := FBloco_0;
  FBloco_D.Bloco_0 := FBloco_0;
  FBloco_F.Bloco_0 := FBloco_0;
  FBloco_I.Bloco_0 := FBloco_0;
  FBloco_M.Bloco_0 := FBloco_0;
  FBloco_P.Bloco_0 := FBloco_0;

  FPath := ExtractFilePath(ParamStr(0));
  Delimitador := '|';   	//Não chamamos a variável diretamente pois precisa-se alterar os registros filhos também.
  CurMascara := '#0.00';	//Não chamamos a variável diretamente pois precisa-se alterar os registros filhos também.
  TrimString := True;		//Não chamamos a variável diretamente pois precisa-se alterar os registros filhos também.

  FEventsBloco_C := TEventsBloco_C.Create(Self);
  FEventsBloco_C.Name := 'EventsBloco_C';
  FEventsBloco_C.SetSubComponent(True);

end;

destructor TACBrSPEDPisCofins.Destroy;
begin
  FACBrTXT.Free;

  FEventsBloco_C.Free;

  FBloco_0.Free;
  FBloco_1.Free;
  FBloco_9.Free;
  FBloco_A.Free;
  FBloco_C.Free;
  FBloco_D.Free;
  FBloco_F.Free;
  FBloco_I.Free;
  FBloco_M.Free;
  FBloco_P.Free;
  inherited;
end;

procedure TACBrSPEDPisCofins.LimpaRegistros;
begin
  FBloco_0.LimpaRegistros;
  FBloco_1.LimpaRegistros;
  FBloco_9.LimpaRegistros;
  FBloco_A.LimpaRegistros;
  FBloco_C.LimpaRegistros;
  FBloco_D.LimpaRegistros;
  FBloco_F.LimpaRegistros;
  FBloco_I.LimpaRegistros;
  FBloco_M.LimpaRegistros;
  FBloco_P.LimpaRegistros;

  // Define valores iniciais as propriedades no create e após limpar os registros.
  IniciaDados;
end;

function TACBrSPEDPisCofins.GetAbout: String;
begin
   Result := 'ACBrSpedFiscal Ver: ' + CACBrSpedPisCofins_Versao;
end;

function TACBrSPEDPisCofins.GetConteudo: TStringList;
begin
  Result := FACBrTXT.Conteudo;
end;

function TACBrSPEDPisCofins.GetDelimitador: String;
begin
   Result := FDelimitador;
end;

function TACBrSPEDPisCofins.GetLinhasBuffer: Integer;
begin
   Result := FACBrTXT.LinhasBuffer ;
end;

procedure TACBrSPEDPisCofins.SetDelimitador(const Value: String);
begin
  FDelimitador := Value;

  FBloco_0.Delimitador := Value;
  FBloco_1.Delimitador := Value;
  FBloco_9.Delimitador := Value;
  FBloco_A.Delimitador := Value;
  FBloco_C.Delimitador := Value;
  FBloco_D.Delimitador := Value;
  FBloco_F.Delimitador := Value;
  FBloco_I.Delimitador := Value;
  FBloco_M.Delimitador := Value;
  FBloco_P.Delimitador := Value;
end;

procedure TACBrSPEDPisCofins.SetLinhasBuffer(const AValue: Integer);
begin
   FACBrTXT.LinhasBuffer := AValue ;
end;

procedure TACBrSPEDPisCofins.SetPath(const AValue: String);
begin
  FPath := PathWithDelim( AValue );
end;

function TACBrSPEDPisCofins.GetCurMascara: String;
begin
  Result := FCurMascara;
end;

procedure TACBrSPEDPisCofins.SetCurMascara(const Value: String);
begin
  FCurMascara := Value;

  FBloco_0.CurMascara := Value;
  FBloco_1.CurMascara := Value;
  FBloco_9.CurMascara := Value;
  FBloco_A.CurMascara := Value;
  FBloco_C.CurMascara := Value;
  FBloco_D.CurMascara := Value;
  FBloco_F.CurMascara := Value;
  FBloco_I.CurMascara := Value;
  FBloco_M.CurMascara := Value;
  FBloco_P.CurMascara := Value;
end;

function TACBrSPEDPisCofins.GetTrimString: boolean;
begin
  Result := FTrimString;
end;

procedure TACBrSPEDPisCofins.SetTrimString(const Value: boolean);
begin
  FTrimString := Value;

  FBloco_0.TrimString := Value;
  FBloco_1.TrimString := Value;
  FBloco_9.TrimString := Value;
  FBloco_A.TrimString := Value;
  FBloco_C.TrimString := Value;
  FBloco_D.TrimString := Value;
  FBloco_F.TrimString := Value;
  FBloco_I.TrimString := Value;
  FBloco_M.TrimString := Value;
  FBloco_P.TrimString := Value;
end;

function TACBrSPEDPisCofins.GetDT_INI: TDateTime;
begin
  Result := FDT_INI;
end;

procedure TACBrSPEDPisCofins.InicializaBloco( Bloco: TACBrSPED ) ;
begin
   Bloco.NomeArquivo  := FACBrTXT.NomeArquivo;
   Bloco.LinhasBuffer := FACBrTXT.LinhasBuffer;
   Bloco.Gravado      := False ;
   Bloco.Conteudo.Clear;
end;

procedure TACBrSPEDPisCofins.IniciaDados;
begin
  // FBloco_0 terá que ter dados
  FBloco_A.RegistroA001.IND_MOV := imSemDados;
  FBloco_C.RegistroC001.IND_MOV := imSemDados;
  FBloco_D.RegistroD001.IND_MOV := imSemDados;
  FBloco_F.RegistroF001.IND_MOV := imSemDados;
  FBloco_I.RegistroI001.IND_MOV := imSemDados;
  FBloco_M.RegistroM001.IND_MOV := imSemDados;
  FBloco_P.RegistroP001.IND_MOV := imSemDados;
  FBloco_1.Registro1001.IND_MOV := imSemDados;
  // FBloco_9 terá que ter dados
end;

procedure TACBrSPEDPisCofins.IniciaGeracao;
begin
  if FInicializado then exit ;

  if FDT_INI = 0 then
    raise Exception.Create(ACBrStr('Informe a data inicial das informações contidas no arquivo!'));

  if FDT_FIN = 0 then
    raise Exception.Create(ACBrStr('Informe a data final das informações contidas no arquivo!'));

  if (Trim(FArquivo) = '') or (Trim(FPath) = '') then
    raise Exception.Create(ACBrStr('Caminho ou nome do arquivo não informado!'));

  FACBrTXT.NomeArquivo := FPath + FArquivo ;
  FACBrTXT.Reset;    // Apaga o Arquivo e limpa memória

  InicializaBloco( Bloco_0 ) ;
  InicializaBloco( Bloco_A ) ;
  InicializaBloco( Bloco_C ) ;
  InicializaBloco( Bloco_D ) ;
  InicializaBloco( Bloco_F ) ;
  InicializaBloco( Bloco_I ) ;
  InicializaBloco( Bloco_M ) ;
  InicializaBloco( Bloco_P ) ;
  InicializaBloco( Bloco_1 ) ;
  InicializaBloco( Bloco_9 ) ;

  ///
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
  Bloco_0.Registro0990.QTD_LIN_0 := 0;
  Bloco_1.Registro1990.QTD_LIN_1 := 0;
  Bloco_A.RegistroA990.QTD_LIN_A := 0;
  Bloco_C.RegistroC990.QTD_LIN_C := 0;
  Bloco_D.RegistroD990.QTD_LIN_D := 0;
  Bloco_F.RegistroF990.QTD_LIN_F := 0;
  Bloco_I.RegistroI990.QTD_LIN_I := 0;
  Bloco_M.RegistroM990.QTD_LIN_M := 0;
  Bloco_P.RegistroP990.QTD_LIN_P := 0;
  Bloco_9.Registro9990.QTD_LIN_9 := 0;
  Bloco_9.Registro9999.QTD_LIN   := 0;

  /// Limpa a lista
  Bloco_9.Registro9900.Clear;

  FInicializado := True ;
end;

procedure TACBrSPEDPisCofins.SetArquivo(const AValue: String);
var
  APath : String;
begin
  if FArquivo = AValue then
     exit;

  FArquivo := ExtractFileName( AValue );
  APath    := ExtractFilePath( AValue );

  if APath <> '' then
     Path := APath;
end;

procedure TACBrSPEDPisCofins.SetDT_INI(const Value: TDateTime);
begin
  FDT_INI := Value;

  FBloco_0.DT_INI := Value;
  FBloco_1.DT_INI := Value;
  FBloco_9.DT_INI := Value;
  FBloco_A.DT_INI := Value;
  FBloco_C.DT_INI := Value;
  FBloco_D.DT_INI := Value;
  FBloco_F.DT_INI := Value;
  FBloco_I.DT_INI := Value;
  FBloco_M.DT_INI := Value;
  FBloco_P.DT_INI := Value;

  if Assigned(FBloco_0) then
  begin
    FBloco_0.Registro0000.DT_INI := Value;
    //     FBloco_E.RegistroE100.DT_INI := Value;
  end;
end;

function TACBrSPEDPisCofins.GetDT_FIN: TDateTime;
begin
  Result := FDT_FIN;
end;

procedure TACBrSPEDPisCofins.SetDT_FIN(const Value: TDateTime);
begin
  FDT_FIN := Value;

  FBloco_0.DT_FIN := Value;
  FBloco_1.DT_FIN := Value;
  FBloco_9.DT_FIN := Value;
  FBloco_A.DT_FIN := Value;
  FBloco_C.DT_FIN := Value;
  FBloco_D.DT_FIN := Value;
  FBloco_F.DT_FIN := Value;
  FBloco_I.DT_FIN := Value;
  FBloco_M.DT_FIN := Value;
  FBloco_P.DT_FIN := Value;

  if Assigned(FBloco_0) then
  begin
    FBloco_0.Registro0000.DT_FIN := Value;
    //     FBloco_E.RegistroE100.DT_FIN := Value;
  end;
end;

function TACBrSPEDPisCofins.GetOnError: TErrorEvent;
begin
  Result := FOnError;
end;

procedure TACBrSPEDPisCofins.SetOnError(const Value: TErrorEvent);
begin
  FOnError := Value;

  FBloco_0.OnError := Value;
  FBloco_1.OnError := Value;
  FBloco_9.OnError := Value;
  FBloco_A.OnError := Value;
  FBloco_C.OnError := Value;
  FBloco_D.OnError := Value;
  FBloco_F.OnError := Value;
  FBloco_I.OnError := Value;
  FBloco_M.OnError := Value;
  FBloco_P.OnError := Value;
end;

procedure TACBrSPEDPisCofins.SaveFileTXT;
begin
  try
    IniciaGeracao;

    WriteBloco_0;
    if FBloco_A.RegistroA001.IND_MOV = imComDados then WriteBloco_A( True );
    if FBloco_C.RegistroC001.IND_MOV = imComDados then WriteBloco_C( True );
    if FBloco_D.RegistroD001.IND_MOV = imComDados then WriteBloco_D;
    if FBloco_F.RegistroF001.IND_MOV = imComDados then WriteBloco_F;
    if FBloco_I.RegistroI001.IND_MOV = imComDados then WriteBloco_I;
    if FBloco_M.RegistroM001.IND_MOV = imComDados then WriteBloco_M;
    if FBloco_P.RegistroP001.IND_MOV = imComDados then WriteBloco_P;
    if FBloco_1.Registro1001.IND_MOV = imComDados then WriteBloco_1;
    WriteBloco_9;
  finally
    /// Limpa de todos os Blocos as listas de todos os registros.
    LimpaRegistros;
    FACBrTXT.Conteudo.Clear;

    FInicializado := False ;
  end;
end;

procedure TACBrSPEDPisCofins.WriteBloco_0;
begin
  if Bloco_0.Gravado then exit ;

  if not FInicializado then
     raise Exception.Create( 'Métodos "IniciaGeracao" não foi executado' );

  /// BLOCO 0
  WriteRegistro0000;
  WriteRegistro0001;
  WriteRegistro0990;
  Bloco_0.WriteBuffer;
  Bloco_0.Conteudo.Clear;
  Bloco_0.Gravado := True ;
end;

procedure TACBrSPEDPisCofins.WriteBloco_A( FechaBloco : Boolean );
begin
   if Bloco_A.Gravado then exit ;

   if not Bloco_0.Gravado then
      WriteBloco_0;

   /// BLOCO A
   WriteRegistroA001;

   if Bloco_A.RegistroA001.IND_MOV = imSemDados then
      FechaBloco := True ;

   if FechaBloco then
      WriteRegistroA990;

   Bloco_A.WriteBuffer;
   Bloco_A.Conteudo.Clear;
   Bloco_A.Gravado := FechaBloco;
end;

procedure TACBrSPEDPisCofins.WriteBloco_C( FechaBloco : Boolean );
begin
   if Bloco_C.Gravado then exit ;

   if not Bloco_A.Gravado then
      WriteBloco_A(True);

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

procedure TACBrSPEDPisCofins.WriteBloco_D;
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

procedure TACBrSPEDPisCofins.WriteBloco_F;
begin
   if Bloco_F.Gravado then exit ;

   if not Bloco_D.Gravado then
      WriteBloco_D;

   /// BLOCO E
   WriteRegistroF001;
   WriteRegistroF990;
   Bloco_F.WriteBuffer;
   Bloco_F.Conteudo.Clear;
   Bloco_F.Gravado := True ;
end;

procedure TACBrSPEDPisCofins.WriteBloco_I;
begin
  if Self.DT_INI >= EncodeDate(2014,01,01) then
  begin
    if Bloco_I.Gravado then exit ;

     if not Bloco_F.Gravado then
        WriteBloco_F;

     /// BLOCO I
     WriteRegistroI001;
     WriteRegistroI990;
     Bloco_I.WriteBuffer;
     Bloco_I.Conteudo.Clear;
     Bloco_I.Gravado := True ;
  end;
end;

procedure TACBrSPEDPisCofins.WriteBloco_M;
begin
   if Bloco_M.Gravado then exit ;

   if not Bloco_F.Gravado then
      WriteBloco_F;

   /// BLOCO G
   WriteRegistroM001;
   WriteRegistroM990;
   Bloco_M.WriteBuffer;
   Bloco_M.Conteudo.Clear;
   Bloco_M.Gravado := True ;
end;

procedure TACBrSPEDPisCofins.WriteBloco_P;
begin
   if not Bloco_M.Gravado then
      WriteBloco_M;

   if (Bloco_P.Gravado) or (Bloco_0.Registro0145Count = 0) then exit ;

   /// BLOCO P
   WriteRegistroP001;
   WriteRegistroP990;
   Bloco_P.WriteBuffer;
   Bloco_P.Conteudo.Clear;
   Bloco_P.Gravado := True ;
end;

procedure TACBrSPEDPisCofins.WriteBloco_1;
begin
   if Bloco_1.Gravado then exit ;

   if not Bloco_P.Gravado then
      WriteBloco_P ;           

   /// BLOCO 1
   WriteRegistro1001;
   WriteRegistro1990;
   Bloco_1.WriteBuffer;
   Bloco_1.Conteudo.Clear;
   Bloco_1.Gravado := True ;
end;

procedure TACBrSPEDPisCofins.WriteBloco_9;
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

procedure TACBrSPEDPisCofins.WriteRegistro0000;
begin
   with Bloco_9.Registro9900.New do
   begin
      REG_BLC := '0000';
      QTD_REG_BLC := 1;
   end;
   Bloco_0.WriteRegistro0000;
end;

procedure TACBrSPEDPisCofins.WriteRegistro0001;
begin
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
   if Bloco_0.Registro0001.IND_MOV = imComDados then
   begin
      with Bloco_9.Registro9900 do
      begin
         if Bloco_0.Registro0035Count > 0 then
         begin
            with New do
            begin
               REG_BLC := '0035';
               QTD_REG_BLC := Bloco_0.Registro0035Count;
            end;
         end;
         if Bloco_0.Registro0100Count > 0 then
         begin
            with New do
            begin
               REG_BLC := '0100';
               QTD_REG_BLC := Bloco_0.Registro0100Count;
            end;
         end;
         if Bloco_0.Registro0110Count > 0 then
         begin
            with New do
            begin
               REG_BLC := '0110';
               QTD_REG_BLC := Bloco_0.Registro0110Count;
            end;
         end;
         if Bloco_0.Registro0111Count > 0 then
         begin
            with New do
            begin
               REG_BLC := '0111';
               QTD_REG_BLC := Bloco_0.Registro0111Count;
            end;
         end;
         if Bloco_0.Registro0120Count > 0 then
         begin
            with New do
            begin
               REG_BLC := '0120';
               QTD_REG_BLC := Bloco_0.Registro0120Count;
            end;
         end;
         if Bloco_0.Registro0140Count > 0 then
         begin
            with New do
            begin
               REG_BLC := '0140';
               QTD_REG_BLC := Bloco_0.Registro0140Count;
            end;
         end;
         if Bloco_0.Registro0145Count > 0 then
         begin
            with New do
            begin
               REG_BLC := '0145';
               QTD_REG_BLC := Bloco_0.Registro0145Count;
            end;
         end;
         if Bloco_0.Registro0150Count > 0 then
         begin
            with New do
            begin
               REG_BLC := '0150';
               QTD_REG_BLC := Bloco_0.Registro0150Count;
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
         if Bloco_0.Registro0208Count > 0 then
         begin
            with New do
            begin
               REG_BLC := '0208';
               QTD_REG_BLC := Bloco_0.Registro0208Count;
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

procedure TACBrSPEDPisCofins.WriteRegistro0990;
begin
   with Bloco_9.Registro9900.New do
   begin
      REG_BLC := '0990';
      QTD_REG_BLC := 1;
   end;
   Bloco_0.WriteRegistro0990;
end;

procedure TACBrSPEDPisCofins.WriteRegistro1001;
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
         if Bloco_1.Registro1020Count > 0 then
         begin
            with New do
            begin
               REG_BLC := '1020';
               QTD_REG_BLC := Bloco_1.Registro1020Count;
            end;
         end;
         if Bloco_1.Registro1050Count > 0 then
         begin
            with New do
            begin
               REG_BLC := '1050';
               QTD_REG_BLC := Bloco_1.Registro1050Count;
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
         if Bloco_1.Registro1101Count > 0 then
         begin
            with New do
            begin
               REG_BLC := '1101';
               QTD_REG_BLC := Bloco_1.Registro1101Count;
            end;
         end;
         if Bloco_1.Registro1102Count > 0 then
         begin
            with New do
            begin
               REG_BLC := '1102';
               QTD_REG_BLC := Bloco_1.Registro1102Count;
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
         if Bloco_1.Registro1220Count > 0 then
         begin
            with New do
            begin
               REG_BLC := '1220';
               QTD_REG_BLC := Bloco_1.Registro1220Count;
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
         if Bloco_1.Registro1500Count > 0 then
         begin
            with New do
            begin
               REG_BLC := '1500';
               QTD_REG_BLC := Bloco_1.Registro1500Count;
            end;
         end;
         if Bloco_1.Registro1501Count > 0 then
         begin
            with New do
            begin
               REG_BLC := '1501';
               QTD_REG_BLC := Bloco_1.Registro1501Count;
            end;
         end;
         if Bloco_1.Registro1502Count > 0 then
         begin
            with New do
            begin
               REG_BLC := '1502';
               QTD_REG_BLC := Bloco_1.Registro1502Count;
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
         if Bloco_1.Registro1610Count > 0 then
         begin
            with New do
            begin
               REG_BLC := '1610';
               QTD_REG_BLC := Bloco_1.Registro1610Count;
            end;
         end;
         if Bloco_1.Registro1620Count > 0 then
         begin
            with New do
            begin
               REG_BLC := '1620';
               QTD_REG_BLC := Bloco_1.Registro1620Count;
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
         if Bloco_1.Registro1800Count > 0 then
         begin
            with New do
            begin
               REG_BLC := '1800';
               QTD_REG_BLC := Bloco_1.Registro1800Count;
            end;
         end;
         if Bloco_1.Registro1809Count > 0 then
         begin
            with New do
            begin
               REG_BLC := '1809';
               QTD_REG_BLC := Bloco_1.Registro1809Count;
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
      end;
   end;
end;

procedure TACBrSPEDPisCofins.WriteRegistro1990;
begin
   with Bloco_9.Registro9900.New do
   begin
      REG_BLC := '1990';
      QTD_REG_BLC := 1;
   end;
   Bloco_1.WriteRegistro1990;
end;

procedure TACBrSPEDPisCofins.WriteRegistroA001;
begin
   Bloco_A.WriteRegistroA001;
end;

procedure TACBrSPEDPisCofins.WriteRegistroA990;
begin
   with Bloco_9.Registro9900 do
   begin
      with New do
      begin
         REG_BLC := 'A001';
         QTD_REG_BLC := 1;
      end;
   end;
   if Bloco_A.RegistroA001.IND_MOV = imComDados then
   begin
      with Bloco_9.Registro9900 do
      begin
         if Bloco_A.RegistroA010Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'A010';
               QTD_REG_BLC := Bloco_A.RegistroA010Count;
            end;
         end;
         if Bloco_A.RegistroA100Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'A100';
               QTD_REG_BLC := Bloco_A.RegistroA100Count;
            end;
         end;
         if Bloco_A.RegistroA110Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'A110';
               QTD_REG_BLC := Bloco_A.RegistroA110Count;
            end;
         end;
         if Bloco_A.RegistroA111Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'A111';
               QTD_REG_BLC := Bloco_A.RegistroA111Count;
            end;
         end;
         if Bloco_A.RegistroA120Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'A120';
               QTD_REG_BLC := Bloco_A.RegistroA120Count;
            end;
         end;
         if Bloco_A.RegistroA170Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'A170';
               QTD_REG_BLC := Bloco_A.RegistroA170Count;
            end;
         end;
      end;
   end;

   with Bloco_9.Registro9900.New do
   begin
      REG_BLC := 'A990';
      QTD_REG_BLC := 1;
   end;
   Bloco_A.WriteRegistroA990;
end;

procedure TACBrSPEDPisCofins.WriteRegistroC001;
begin
  Bloco_C.WriteRegistroC001;
end;

procedure TACBrSPEDPisCofins.WriteRegistroC990;
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
       if Bloco_C.RegistroC010Count > 0 then
       begin
         with New do
         begin
           REG_BLC := 'C010';
           QTD_REG_BLC := Bloco_C.RegistroC010Count;
         end;
       end;
       if Bloco_C.RegistroC100Count > 0 then
       begin
         with New do
         begin
           REG_BLC := 'C100';
           QTD_REG_BLC := Bloco_C.RegistroC100Count;
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
       if Bloco_C.RegistroC120Count > 0 then
       begin
         with New do
         begin
           REG_BLC := 'C120';
           QTD_REG_BLC := Bloco_C.RegistroC120Count;
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
       if Bloco_C.RegistroC175Count > 0 then
       begin
         with New do
         begin
           REG_BLC := 'C175';
           QTD_REG_BLC := Bloco_C.RegistroC175Count;
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
       if Bloco_C.RegistroC188Count > 0 then
       begin
         with New do
         begin
           REG_BLC := 'C188';
           QTD_REG_BLC := Bloco_C.RegistroC188Count;
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
       if Bloco_C.RegistroC198Count > 0 then
       begin
         with New do
         begin
           REG_BLC := 'C198';
           QTD_REG_BLC := Bloco_C.RegistroC198Count;
         end;
       end;
       if Bloco_C.RegistroC199Count > 0 then
       begin
         with New do
         begin
           REG_BLC := 'C199';
           QTD_REG_BLC := Bloco_C.RegistroC199Count;
         end;
       end;
       if Bloco_C.RegistroC380Count > 0 then
       begin
         with New do
         begin
           REG_BLC := 'C380';
           QTD_REG_BLC := Bloco_C.RegistroC380Count;
         end;
       end;
       if Bloco_C.RegistroC381Count > 0 then
       begin
         with New do
         begin
           REG_BLC := 'C381';
           QTD_REG_BLC := Bloco_C.RegistroC381Count;
         end;
       end;
       if Bloco_C.RegistroC385Count > 0 then
       begin
         with New do
         begin
           REG_BLC := 'C385';
           QTD_REG_BLC := Bloco_C.RegistroC385Count;
         end;
       end;
       if Bloco_C.RegistroC395Count > 0 then
       begin
         with New do
         begin
           REG_BLC := 'C395';
           QTD_REG_BLC := Bloco_C.RegistroC395Count;
         end;
       end;
       if Bloco_C.RegistroC396Count > 0 then
       begin
         with New do
         begin
           REG_BLC := 'C396';
           QTD_REG_BLC := Bloco_C.RegistroC396Count;
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
       if Bloco_C.RegistroC481Count > 0 then
       begin
         with New do
         begin
           REG_BLC := 'C481';
           QTD_REG_BLC := Bloco_C.RegistroC481Count;
         end;
       end;
       if Bloco_C.RegistroC485Count > 0 then
       begin
         with New do
         begin
           REG_BLC := 'C485';
           QTD_REG_BLC := Bloco_C.RegistroC485Count;
         end;
       end;
       if Bloco_C.RegistroC489Count > 0 then
       begin
         with New do
         begin
           REG_BLC := 'C489';
           QTD_REG_BLC := Bloco_C.RegistroC489Count;
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
       if Bloco_C.RegistroC491Count > 0 then
       begin
         with New do
         begin
           REG_BLC := 'C491';
           QTD_REG_BLC := Bloco_C.RegistroC491Count;
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
       if Bloco_C.RegistroC499Count > 0 then
       begin
         with New do
         begin
           REG_BLC := 'C499';
           QTD_REG_BLC := Bloco_C.RegistroC499Count;
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
       if Bloco_C.RegistroC501Count > 0 then
       begin
         with New do
         begin
           REG_BLC := 'C501';
           QTD_REG_BLC := Bloco_C.RegistroC501Count;
         end;
       end;
       if Bloco_C.RegistroC505Count > 0 then
       begin
         with New do
         begin
           REG_BLC := 'C505';
           QTD_REG_BLC := Bloco_C.RegistroC505Count;
         end;
       end;
       if Bloco_C.RegistroC509Count > 0 then
       begin
         with New do
         begin
           REG_BLC := 'C509';
           QTD_REG_BLC := Bloco_C.RegistroC509Count;
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
       if Bloco_C.RegistroC605Count > 0 then
       begin
         with New do
         begin
           REG_BLC := 'C605';
           QTD_REG_BLC := Bloco_C.RegistroC605Count;
         end;
       end;
       if Bloco_C.RegistroC609Count > 0 then
       begin
         with New do
         begin
           REG_BLC := 'C609';
           QTD_REG_BLC := Bloco_C.RegistroC609Count;
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
       end;
       if Bloco_C.RegistroC880Count > 0 then
       begin
         with New do
         begin
           REG_BLC := 'C880';
           QTD_REG_BLC := Bloco_C.RegistroC880Count;
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

procedure TACBrSPEDPisCofins.WriteRegistroD001;
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
         if Bloco_D.RegistroD010Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'D010';
               QTD_REG_BLC := Bloco_D.RegistroD010Count;
            end;
         end;
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
         if Bloco_D.RegistroD105Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'D105';
               QTD_REG_BLC := Bloco_D.RegistroD105Count;
            end;
         end;
         if Bloco_D.RegistroD111Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'D111';
               QTD_REG_BLC := Bloco_D.RegistroD111Count;
            end;
         end;
         if Bloco_D.RegistroD200Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'D200';
               QTD_REG_BLC := Bloco_D.RegistroD200Count;
            end;
         end;
         if Bloco_D.RegistroD201Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'D201';
               QTD_REG_BLC := Bloco_D.RegistroD201Count;
            end;
         end;
         if Bloco_D.RegistroD205Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'D205';
               QTD_REG_BLC := Bloco_D.RegistroD205Count;
            end;
         end;
         if Bloco_D.RegistroD209Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'D209';
               QTD_REG_BLC := Bloco_D.RegistroD209Count;
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
         if Bloco_D.RegistroD309Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'D309';
               QTD_REG_BLC := Bloco_D.RegistroD309Count;
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
         if Bloco_D.RegistroD359Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'D359';
               QTD_REG_BLC := Bloco_D.RegistroD359Count;
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
         if Bloco_D.RegistroD501Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'D501';
               QTD_REG_BLC := Bloco_D.RegistroD501Count;
            end;
         end;
         if Bloco_D.RegistroD505Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'D505';
               QTD_REG_BLC := Bloco_D.RegistroD505Count;
            end;
         end;
         if Bloco_D.RegistroD509Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'D509';
               QTD_REG_BLC := Bloco_D.RegistroD509Count;
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
         if Bloco_D.RegistroD601Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'D601';
               QTD_REG_BLC := Bloco_D.RegistroD601Count;
            end;
         end;
         if Bloco_D.RegistroD605Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'D605';
               QTD_REG_BLC := Bloco_D.RegistroD605Count;
            end;
         end;
         if Bloco_D.RegistroD609Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'D609';
               QTD_REG_BLC := Bloco_D.RegistroD609Count;
            end;
         end;
      end;
   end;
end;

procedure TACBrSPEDPisCofins.WriteRegistroD990;
begin
  with Bloco_9.Registro9900.New do
  begin
    REG_BLC := 'D990';
    QTD_REG_BLC := 1;
  end;
  Bloco_D.WriteRegistroD990;
end;

procedure TACBrSPEDPisCofins.WriteRegistroF001;
begin
   Bloco_F.WriteRegistroF001;
   //
   with Bloco_9.Registro9900 do
   begin
      with New do
      begin
         REG_BLC := 'F001';
         QTD_REG_BLC := 1;
      end;
   end;
   if Bloco_F.RegistroF001.IND_MOV = imComDados then
   begin
      with Bloco_9.Registro9900 do
      begin
         if Bloco_F.RegistroF010Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'F010';
               QTD_REG_BLC := Bloco_F.RegistroF010Count;
            end;
         end;
         if Bloco_F.RegistroF100Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'F100';
               QTD_REG_BLC := Bloco_F.RegistroF100Count;
            end;
         end;
         if Bloco_F.RegistroF111Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'F111';
               QTD_REG_BLC := Bloco_F.RegistroF111Count;
            end;
         end;
         if Bloco_F.RegistroF120Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'F120';
               QTD_REG_BLC := Bloco_F.RegistroF120Count;
            end;
         end;
         if Bloco_F.RegistroF129Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'F129';
               QTD_REG_BLC := Bloco_F.RegistroF129Count;
            end;
         end;
         if Bloco_F.RegistroF130Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'F130';
               QTD_REG_BLC := Bloco_F.RegistroF130Count;
            end;
         end;
         if Bloco_F.RegistroF139Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'F139';
               QTD_REG_BLC := Bloco_F.RegistroF139Count;
            end;
         end;
         if Bloco_F.RegistroF150Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'F150';
               QTD_REG_BLC := Bloco_F.RegistroF150Count;
            end;
         end;
         if Bloco_F.RegistroF200Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'F200';
               QTD_REG_BLC := Bloco_F.RegistroF200Count;
            end;
         end;
         if Bloco_F.RegistroF205Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'F205';
               QTD_REG_BLC := Bloco_F.RegistroF205Count;
            end;
         end;
         if Bloco_F.RegistroF210Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'F210';
               QTD_REG_BLC := Bloco_F.RegistroF210Count;
            end;
         end;
         if Bloco_F.RegistroF211Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'F211';
               QTD_REG_BLC := Bloco_F.RegistroF211Count;
            end;
         end;
         (*Edilon Alves de Oliveira*)
         if Bloco_F.RegistroF500Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'F500';
               QTD_REG_BLC := Bloco_F.RegistroF500Count;
            end;
         end;
         if Bloco_F.RegistroF510Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'F510';
               QTD_REG_BLC := Bloco_F.RegistroF510Count;
            end;
         end;
         (*Adilson Rodrigues - foi incluso este código para totalizar o registro f525 no blobo 9*)
         if Bloco_F.RegistroF525Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'F525';
               QTD_REG_BLC := Bloco_F.RegistroF525Count;
            end;
         end;
         if Bloco_F.RegistroF550Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'F550';
               QTD_REG_BLC := Bloco_F.RegistroF550Count;
            end;
         end;
         if Bloco_F.RegistroF560Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'F560';
               QTD_REG_BLC := Bloco_F.RegistroF560Count;
            end;
         end;
         if Bloco_F.RegistroF600Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'F600';
               QTD_REG_BLC := Bloco_F.RegistroF600Count;
            end;
         end;
         if Bloco_F.RegistroF700Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'F700';
               QTD_REG_BLC := Bloco_F.RegistroF700Count;
            end;
         end;
         if Bloco_F.RegistroF800Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'F800';
               QTD_REG_BLC := Bloco_F.RegistroF800Count;
            end;
         end;
      end;
   end;
end;

procedure TACBrSPEDPisCofins.WriteRegistroF990;
begin
  with Bloco_9.Registro9900.New do
  begin
    REG_BLC := 'F990';
    QTD_REG_BLC := 1;
  end;
  Bloco_F.WriteRegistroF990;
end;

procedure TACBrSPEDPisCofins.WriteRegistroI001;
begin
  Bloco_I.WriteRegistroI001;
  //
     with Bloco_9.Registro9900 do
     begin
        with New do
        begin
           REG_BLC := 'I001';
           QTD_REG_BLC := 1;
        end;
     end;
     if Bloco_I.RegistroI001.IND_MOV = imComDados then
     begin
        with Bloco_9.Registro9900 do
        begin
           if Bloco_I.RegistroI010Count > 0 then
           begin
              with New do
              begin
                 REG_BLC := 'I010';
                 QTD_REG_BLC := Bloco_I.RegistroI010Count;
              end;
           end;
           if Bloco_I.RegistroI100Count > 0 then
           begin
              with New do
              begin
                 REG_BLC := 'I100';
                 QTD_REG_BLC := Bloco_I.RegistroI100Count;
              end;
           end;
           if Bloco_I.RegistroI200Count > 0 then
           begin
              with New do
              begin
                 REG_BLC := 'I200';
                 QTD_REG_BLC := Bloco_I.RegistroI200Count;
              end;
           end;
           if Bloco_I.RegistroI299Count > 0 then
           begin
              with New do
              begin
                 REG_BLC := 'I299';
                 QTD_REG_BLC := Bloco_I.RegistroI299Count;
              end;
           end;
           if Bloco_I.RegistroI300Count > 0 then
           begin
              with New do
              begin
                 REG_BLC := 'I300';
                 QTD_REG_BLC := Bloco_I.RegistroI300Count;
              end;
           end;
           if Bloco_I.RegistroI399Count > 0 then
           begin
              with New do
              begin
                 REG_BLC := 'I399';
                 QTD_REG_BLC := Bloco_I.RegistroI399Count;
              end;
           end;
        end;
     end;
end;

procedure TACBrSPEDPisCofins.WriteRegistroI990;
begin
  with Bloco_9.Registro9900.New do
  begin
    REG_BLC := 'I990';
    QTD_REG_BLC := 1;
  end;
  Bloco_I.WriteRegistroI990;
end;

procedure TACBrSPEDPisCofins.WriteRegistroM001;
begin
   Bloco_M.WriteRegistroM001;
   //
   with Bloco_9.Registro9900 do
   begin
      with New do
      begin
         REG_BLC := 'M001';
         QTD_REG_BLC := 1;
      end;
   end;
   if Bloco_M.RegistroM001.IND_MOV = imComDados then
   begin
      with Bloco_9.Registro9900 do
      begin
         if Bloco_M.RegistroM100Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'M100';
               QTD_REG_BLC := Bloco_M.RegistroM100Count;
            end;
         end;
         if Bloco_M.RegistroM105Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'M105';
               QTD_REG_BLC := Bloco_M.RegistroM105Count;
            end;
         end;
         if Bloco_M.RegistroM110Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'M110';
               QTD_REG_BLC := Bloco_M.RegistroM110Count;
            end;
         end;
         if Bloco_M.RegistroM115Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'M115';
               QTD_REG_BLC := Bloco_M.RegistroM115Count;
            end;
         end;
         if Bloco_M.RegistroM200Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'M200';
               QTD_REG_BLC := Bloco_M.RegistroM200Count;
            end;
         end;
         if Bloco_M.RegistroM205Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'M205';
               QTD_REG_BLC := Bloco_M.RegistroM205Count;
            end;
         end;
         if Bloco_M.RegistroM210Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'M210';
               QTD_REG_BLC := Bloco_M.RegistroM210Count;
            end;
         end;
         if Bloco_M.RegistroM211Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'M211';
               QTD_REG_BLC := Bloco_M.RegistroM211Count;
            end;
         end;
         if Bloco_M.RegistroM215Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'M215';
               QTD_REG_BLC := Bloco_M.RegistroM215Count;
            end;
         end;
         if Bloco_M.RegistroM220Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'M220';
               QTD_REG_BLC := Bloco_M.RegistroM220Count;
            end;
         end;
         if Bloco_M.RegistroM225Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'M225';
               QTD_REG_BLC := Bloco_M.RegistroM225Count;
            end;
         end;
         if Bloco_M.RegistroM230Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'M230';
               QTD_REG_BLC := Bloco_M.RegistroM230Count;
            end;
         end;
         if Bloco_M.RegistroM300Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'M300';
               QTD_REG_BLC := Bloco_M.RegistroM300Count;
            end;
         end;
         if Bloco_M.RegistroM350Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'M350';
               QTD_REG_BLC := Bloco_M.RegistroM350Count;
            end;
         end;
         if Bloco_M.RegistroM400Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'M400';
               QTD_REG_BLC := Bloco_M.RegistroM400Count;
            end;
         end;
         if Bloco_M.RegistroM410Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'M410';
               QTD_REG_BLC := Bloco_M.RegistroM410Count;
            end;
         end;
         if Bloco_M.RegistroM500Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'M500';
               QTD_REG_BLC := Bloco_M.RegistroM500Count;
            end;
         end;
         if Bloco_M.RegistroM505Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'M505';
               QTD_REG_BLC := Bloco_M.RegistroM505Count;
            end;
         end;
         if Bloco_M.RegistroM510Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'M510';
               QTD_REG_BLC := Bloco_M.RegistroM510Count;
            end;
         end;
         if Bloco_M.RegistroM515Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'M515';
               QTD_REG_BLC := Bloco_M.RegistroM515Count;
            end;
         end;
         if Bloco_M.RegistroM600Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'M600';
               QTD_REG_BLC := Bloco_M.RegistroM600Count;
            end;
         end;
         if Bloco_M.RegistroM605Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'M605';
               QTD_REG_BLC := Bloco_M.RegistroM605Count;
            end;
         end;
         if Bloco_M.RegistroM610Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'M610';
               QTD_REG_BLC := Bloco_M.RegistroM610Count;
            end;
         end;
         if Bloco_M.RegistroM611Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'M611';
               QTD_REG_BLC := Bloco_M.RegistroM611Count;
            end;
         end;
         if Bloco_M.RegistroM615Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'M615';
               QTD_REG_BLC := Bloco_M.RegistroM615Count;
            end;
         end;
         if Bloco_M.RegistroM620Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'M620';
               QTD_REG_BLC := Bloco_M.RegistroM620Count;
            end;
         end;
         if Bloco_M.RegistroM625Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'M625';
               QTD_REG_BLC := Bloco_M.RegistroM625Count;
            end;
         end;
         if Bloco_M.RegistroM630Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'M630';
               QTD_REG_BLC := Bloco_M.RegistroM630Count;
            end;
         end;
         if Bloco_M.RegistroM700Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'M700';
               QTD_REG_BLC := Bloco_M.RegistroM700Count;
            end;
         end;
         if Bloco_M.RegistroM800Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'M800';
               QTD_REG_BLC := Bloco_M.RegistroM800Count;
            end;
         end;
         if Bloco_M.RegistroM810Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'M810';
               QTD_REG_BLC := Bloco_M.RegistroM810Count;
            end;
         end;
      end;
   end;
end;

procedure TACBrSPEDPisCofins.WriteRegistroM990;
begin
  with Bloco_9.Registro9900.New do
  begin
    REG_BLC := 'M990';
    QTD_REG_BLC := 1;
  end;
  Bloco_M.WriteRegistroM990;
end;

procedure TACBrSPEDPisCofins.WriteRegistroP001;
begin
   Bloco_P.WriteRegistroP001;
   //
   with Bloco_9.Registro9900 do
   begin
      with New do
      begin
         REG_BLC := 'P001';
         QTD_REG_BLC := 1;
      end;
   end;
   if Bloco_P.RegistroP001.IND_MOV = imComDados then
   begin
      with Bloco_9.Registro9900 do
      begin
         if Bloco_P.RegistroP010Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'P010';
               QTD_REG_BLC := Bloco_P.RegistroP010Count;
            end;
         end;
         if Bloco_P.RegistroP100Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'P100';
               QTD_REG_BLC := Bloco_P.RegistroP100Count;
            end;
         end;
         if Bloco_P.RegistroP110Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'P110';
               QTD_REG_BLC := Bloco_P.RegistroP110Count;
            end;
         end;
         if Bloco_P.RegistroP199Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'P199';
               QTD_REG_BLC := Bloco_P.RegistroP199Count;
            end;
         end;
         if Bloco_P.RegistroP200Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'P200';
               QTD_REG_BLC := Bloco_P.RegistroP200Count;
            end;
         end;
         if Bloco_P.RegistroP210Count > 0 then
         begin
            with New do
            begin
               REG_BLC := 'P210';
               QTD_REG_BLC := Bloco_P.RegistroP210Count;
            end;
         end;
      end;
   end;


end;

procedure TACBrSPEDPisCofins.WriteRegistroP990;
begin
  with Bloco_9.Registro9900.New do
  begin
    REG_BLC := 'P990';
    QTD_REG_BLC := 1;
  end;
  Bloco_P.WriteRegistroP990;
end;

procedure TACBrSPEDPisCofins.WriteRegistro9001;
begin
  with Bloco_9.Registro9900.New do
  begin
    REG_BLC := '9001';
    QTD_REG_BLC := 1;
  end;
  Bloco_9.WriteRegistro9001;
end;

procedure TACBrSPEDPisCofins.WriteRegistro9900;
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

procedure TACBrSPEDPisCofins.WriteRegistro9990;
begin
  Bloco_9.WriteRegistro9990;
end;

procedure TACBrSPEDPisCofins.WriteRegistro9999;
begin
  Bloco_9.Registro9999.QTD_LIN := Bloco_9.Registro9999.QTD_LIN + Bloco_0.Registro0990.QTD_LIN_0 +
                                                                 Bloco_1.Registro1990.QTD_LIN_1 +
                                                                 Bloco_A.RegistroA990.QTD_LIN_A +
                                                                 Bloco_C.RegistroC990.QTD_LIN_C +
                                                                 Bloco_D.RegistroD990.QTD_LIN_D +
                                                                 Bloco_F.RegistroF990.QTD_LIN_F +
                                                                 ifthen(Self.DT_INI >= EncodeDate(2014,01,01), Bloco_I.RegistroI990.QTD_LIN_I,0) +
                                                                 Bloco_M.RegistroM990.QTD_LIN_M +
                                                                 Bloco_P.RegistroP990.QTD_LIN_P +
                                                                 Bloco_9.Registro9990.QTD_LIN_9;
  Bloco_9.WriteRegistro9999;
end;


{$ifdef FPC}
initialization
   {$I ACBrSpedPisCofins.lrs}
{$endif}

end.

