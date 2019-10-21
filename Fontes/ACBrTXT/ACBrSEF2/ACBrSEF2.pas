{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2009   Isaque Pinheiro                      }
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
|* 23/08/2013: Juliana Tamizou
|*  - Distribuição da Primeira Versao
|* 06/05/2013: Juliano Rosa
|*  - Inclusão Registro E120
|* 09/05/2013: Juliano Rosa
|*  - Só grava Bloco H se existir registro de inventário
|*  - Correção contagem dos registros do Bloco E no Registro 9900
*******************************************************************************}
{$I ACBr.inc}

unit ACBrSEF2;

interface

uses
  SysUtils, Classes,
  ACBrBase,
     {$IFNDEF NOGUI}
      {$IFDEF FPC}
       LResources,
      {$ENDIF}
     {$ENDIF}
  ACBrTXTClass, ACBrSEF2_Bloco0_1, ACBrSEF2_BlocoE_1, ACBrSEF2_Bloco9,
  ACBrSEF2_eDoc_BlocoC_Class, ACBrSEF2_BlocoH_1, ACBrSEF2Conversao, ACBrSEF2_BlocoF_1;

type
  TACBrSEF2Arquivo = (aSEF, aEDOC);
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TACBrSEF2 = class(TACBrComponent)
  private
    fACBrTXT: TACBrTXTClass;
    fArquivo: ansistring;
    fInicializado: boolean;
    fOnError: TErrorEvent;

    fDT_INI: TDateTime;        // Data inicial das informações contidas no arquivo
    fDT_FIN: TDateTime;        // Data final das informações contidas no arquivo
    fTipoArquivo: TACBrSEF2Arquivo;
    fPath: ansistring;
    fDelimitador: ansistring;
    fTrimString: boolean;          // Retorna a string sem espaços em branco iniciais e finais
    fCurMascara: ansistring;       // Mascara para valores tipo currency

    fBloco_0: TBloco_0;
    fBloco_E: TBloco_E;
    fBloco_C: TBloco_C;
    fBloco_H: TBloco_H;
    fBloco_F: TBloco_F;
    fBloco_9: TBloco_9;

    function GetConteudo: TStringList;
    function GetDelimitador: ansistring;
    function GetLinhasBuffer: integer;
    function GetTrimString: boolean;
    function GetCurMascara: ansistring;
    function GetDT_FIN: TDateTime;
    function GetDT_INI: TDateTime;
    procedure InicializaBloco(Bloco: TACBrSEFIIEDOC);
    procedure SetArquivo(const Value: ansistring);
    procedure SetDelimitador(const Value: ansistring);
    procedure SetLinhasBuffer(const Value: integer);
    procedure SetPath(const Value: ansistring);
    procedure SetTrimString(const Value: boolean);
    procedure SetCurMascara(const Value: ansistring);
    procedure SetDT_FIN(const Value: TDateTime);
    procedure SetDT_INI(const Value: TDateTime);

    function GetOnError: TErrorEvent;
    procedure SetOnError(const Value: TErrorEvent);

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
    /// BLOCO H
    procedure WriteRegistroH001;
    procedure WriteRegistroH990;

    /// BLOCO F
    procedure WriteRegistroF001;
    procedure WriteRegistroF990;

    /// BLOCO 9
    procedure WriteRegistro9001;
    procedure WriteRegistro9900;
    procedure WriteRegistro9990;
    procedure WriteRegistro9999;

  public
    constructor Create(AOwner: TComponent); override; /// Create
    destructor Destroy; override;

    procedure SaveFileTXT;

    procedure IniciaGeracao;
    procedure WriteBloco_0;
    procedure WriteBloco_C(FechaBloco: boolean);
    procedure WriteBloco_H;
    procedure WriteBloco_F;
    procedure WriteBloco_E;
    procedure WriteBloco_9;

    property Conteudo: TStringList read GetConteudo;

    property DT_INI: TDateTime read GetDT_INI write SetDT_INI;
    property DT_FIN: TDateTime read GetDT_FIN write SetDT_FIN;

    property Bloco_0: TBloco_0 read fBloco_0 write fBloco_0;
    property Bloco_C: TBloco_C read fBloco_C write fBloco_C;
    property Bloco_E: TBloco_E read fBloco_E write fBloco_E;
    property Bloco_H: TBloco_H read fBloco_H write fBloco_H;
    property Bloco_F: TBloco_F read fBloco_F write fBloco_F;
    property Bloco_9: TBloco_9 read fBloco_9 write fBloco_9;
  published
    property TipoArquivo: TACBrSEF2Arquivo read fTipoArquivo write fTipoArquivo default aSEF;
    property Path: ansistring read fPath write SetPath;
    property Arquivo: ansistring read FArquivo write SetArquivo;
    property LinhasBuffer: integer read GetLinhasBuffer write SetLinhasBuffer default 1000;
    property Delimitador: ansistring read GetDelimitador write SetDelimitador;
    property TrimString: boolean read GetTrimString write SetTrimString;
    property CurMascara: ansistring read GetCurMascara write SetCurMascara;

    property OnError: TErrorEvent read GetOnError write SetOnError;

  end;

procedure Register;

implementation

Uses
  DateUtils, ACBrUtil;

{$IFNDEF FPC}
 {$R ACBrSEF2.dcr}
{$ENDIF}

procedure Register;
begin
  RegisterComponents('ACBrTXT', [TACBrSEF2]);
end;


{ TACBrSEF2 }

constructor TACBrSEF2.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  fACBrTXT := TACBrTXTClass.Create;
  fACBrTXT.LinhasBuffer := 1000;

  fInicializado := False;

  fBloco_0 := TBloco_0.Create;
  fBloco_C := TBloco_C.Create;
  fBloco_E := TBloco_E.Create;
  fBloco_F := TBloco_F.Create;
  fBloco_H := TBloco_H.Create;
  fBloco_9 := TBloco_9.Create;

  FPath := ExtractFilePath(ParamStr(0));
  FDelimitador := '|';
  FCurMascara := '#0.00';
  FTrimString := True;
end;

destructor TACBrSEF2.Destroy;
begin
  fACBrTXT.Free;

  fBloco_0.Free;
  fBloco_C.Free;
  fBloco_E.Free;
  fBloco_F.Free;
  fBloco_H.Free;
  fBloco_9.Free;

  inherited;
end;

function TACBrSEF2.GetConteudo: TStringList;
begin
  Result := FACBrTXT.Conteudo;
end;

function TACBrSEF2.GetCurMascara: ansistring;
begin
  Result := FCurMascara;
end;

function TACBrSEF2.GetDelimitador: ansistring;
begin
  Result := fDelimitador;
end;

function TACBrSEF2.GetDT_FIN: TDateTime;
begin
  Result := fDT_FIN;
end;

function TACBrSEF2.GetDT_INI: TDateTime;
begin
  Result := fDT_INI;
end;

function TACBrSEF2.GetLinhasBuffer: integer;
begin
  Result := fACBrTXT.LinhasBuffer;
end;

function TACBrSEF2.GetOnError: TErrorEvent;
begin
  Result := FOnError;
end;

function TACBrSEF2.GetTrimString: boolean;
begin
  Result := FTrimString;
end;

procedure TACBrSEF2.IniciaGeracao;
begin
  if fInicializado then
    exit;

  if (Trim(fArquivo) = '') or (Trim(fPath) = '') then
    raise Exception.Create(ACBrStr('Caminho ou nome do arquivo não informado!'));

  fACBrTXT.NomeArquivo := FPath + FArquivo;
  {Apaga o Arquivo existente e limpa memória}
  fACBrTXT.Reset;

  InicializaBloco(Bloco_0);
  InicializaBloco(Bloco_C);
  InicializaBloco(Bloco_E);
  InicializaBloco(Bloco_F);
  InicializaBloco(Bloco_H);
  InicializaBloco(Bloco_9);

  {Checando o período informado}
  fACBrTXT.Check(fDT_INI > 0, 'CHECAGEM INICIAL: Informe a data inicial das ' +
    'informações contidas no arquivo!');
  fACBrTXT.Check(fDT_FIN > 0, 'CHECAGEM INICIAL: Informe a data  final das ' +
    'final das informações contidas no arquivo!');
  fACBrTXT.Check(DayOf(fDT_INI) = 1, 'CHECAGEM INICIAL: A data inicial deve ' +
    'corresponder ao primeiro dia do mês informado!');
  fACBrTXT.Check(fDT_FIN >= FDT_INI, 'CHECAGEM INICIAL: A data final deve se ' +
    'maior que a data inicial!');
  fACBrTXT.Check(fDT_FIN <= Date, 'CHECAGEM INICIAL: A data final "%s"  não ' +
    'pode ser superior a data atual "%s"!',
    [DateToStr(fDT_FIN), DateToStr(Date)]);
  fACBrTXT.Check(DateOf(EndOfTheMonth(fDT_FIN)) = DateOf(fDT_FIN),
    'CHECAGEM ' + 'INICIAL: A data final deve corresponder ao ' +
    'último dia do mês informado!');

  { Preparação para totalizações de registros. }
  Bloco_0.Registro0990.QTD_LIN_0 := 0;
  Bloco_C.RegistroC990.QTD_LIN_C := 0;
  Bloco_E.RegistroE990.QTD_LIN_E := 0;
  Bloco_F.RegistroF990.QTD_LIN_F := 0;
  Bloco_H.RegistroH990.QTD_LIN_H := 0;
  Bloco_9.Registro9990.QTD_LIN_9 := 0;

  fInicializado := True;
end;

procedure TACBrSEF2.InicializaBloco(Bloco: TACBrSEFIIEDOC);
begin
  Bloco.NomeArquivo := FACBrTXT.NomeArquivo;
  Bloco.LinhasBuffer := FACBrTXT.LinhasBuffer;
  Bloco.Gravado := False;
  Bloco.Conteudo.Clear;
end;

procedure TACBrSEF2.LimpaRegistros;
begin
  FBloco_0.LimpaRegistros;
  FBloco_C.LimpaRegistros;
  FBloco_H.LimpaRegistros;
  fBloco_F.LimpaRegistros;
  FBloco_E.LimpaRegistros;
  FBloco_9.LimpaRegistros;
end;

procedure TACBrSEF2.SaveFileTXT;
begin
  try
    IniciaGeracao;

    WriteBloco_0;

    if fTipoArquivo = aEDOC then
      WriteBloco_C(True);

    if fTipoArquivo = aSEF then
    begin
      WriteBloco_E;

      if (Bloco_F.RegistroF001.RegistroF200.Count > 0) then
        WriteBloco_F;

      if Bloco_H.RegistroH001.IND_DAD = icContConteudo
      then WriteBloco_H;
    end;

    WriteBloco_9;
  finally
    LimpaRegistros;
    fACBrTXT.Conteudo.Clear;
    fInicializado := False;
  end;
end;

procedure TACBrSEF2.SetArquivo(const Value: ansistring);
var
  aPath: ansistring;
begin
  if fArquivo = Value then
    exit;

  fArquivo := ExtractFileName(Value);
  aPath := ExtractFilePath(Value);

  if aPath <> '' then
    Path := aPath;
end;

procedure TACBrSEF2.SetCurMascara(const Value: ansistring);
begin
  fCurMascara := Value;

  fBloco_0.CurMascara := Value;
  fBloco_C.CurMascara := Value;
  fBloco_E.CurMascara := Value;
  fBloco_F.CurMascara := Value;
  fBloco_H.CurMascara := Value;
  fBloco_9.CurMascara := Value;
end;

procedure TACBrSEF2.SetDelimitador(const Value: ansistring);
begin
  fDelimitador := Value;

  fBloco_0.Delimitador := Value;
  fBloco_C.Delimitador := Value;
  fBloco_E.Delimitador := Value;
  fBloco_F.Delimitador := Value;
  fBloco_H.Delimitador := Value;
  fBloco_9.Delimitador := Value;
end;

procedure TACBrSEF2.SetDT_FIN(const Value: TDateTime);
begin
  fDT_FIN := Value;

  fBloco_0.DT_FIN := Value;
  fBloco_C.DT_FIN := Value;
  fBloco_E.DT_FIN := Value;
  fBloco_F.DT_FIN := Value;
  fBloco_H.DT_FIN := Value;
  fBloco_9.DT_FIN := Value;

  if Assigned(fBloco_0) then
    fBloco_0.Registro0000.DT_FIN := Value;
end;

procedure TACBrSEF2.SetDT_INI(const Value: TDateTime);
begin
  fDT_INI := Value;

  fBloco_0.DT_INI := Value;
  fBloco_C.DT_INI := Value;
  fBloco_E.DT_INI := Value;
  fBloco_F.DT_INI := Value;
  fBloco_H.DT_INI := Value;
  fBloco_9.DT_INI := Value;

  if Assigned(FBloco_0) then
    fBloco_0.Registro0000.DT_INI := Value;
end;

procedure TACBrSEF2.SetLinhasBuffer(const Value: integer);
begin
  fACBrTXT.LinhasBuffer := Value;
end;

procedure TACBrSEF2.SetOnError(const Value: TErrorEvent);
begin
  fOnError := Value;

  fBloco_0.OnError := Value;
  fBloco_C.OnError := Value;
  fBloco_E.OnError := Value;
  fBloco_F.OnError := Value;
  fBloco_H.OnError := Value;
  fBloco_9.OnError := Value;
end;

procedure TACBrSEF2.SetPath(const Value: ansistring);
begin
  fPath := PathWithDelim(Value);
end;

procedure TACBrSEF2.SetTrimString(const Value: boolean);
begin
  fTrimString := Value;

  fBloco_0.TrimString := Value;
  fBloco_C.TrimString := Value;
  fBloco_E.TrimString := Value;
  fBloco_F.TrimString := Value;
  fBloco_H.TrimString := Value;
  fBloco_9.TrimString := Value;
end;

procedure TACBrSEF2.WriteBloco_0;
begin
  if Bloco_0.Gravado then
    Exit;

  if not FInicializado then
    raise Exception.Create('Métodos "IniciaGeracao" não foi executado');

  /// BLOCO 0
  WriteRegistro0000;
  WriteRegistro0001;
  WriteRegistro0990;

  Bloco_0.WriteBuffer;
  Bloco_0.Conteudo.Clear;
  Bloco_0.Gravado := True;
end;

procedure TACBrSEF2.WriteBloco_9;
begin
  if Bloco_9.Gravado then
    exit;


  /// BLOCO 9
  WriteRegistro9001;
  WriteRegistro9900;
  WriteRegistro9990;
  WriteRegistro9999;

  Bloco_9.WriteBuffer;
  Bloco_9.Conteudo.Clear;
  Bloco_9.Gravado := True;
end;

procedure TACBrSEF2.WriteBloco_C(FechaBloco: boolean);
begin
  if Bloco_C.Gravado then
    Exit;

  if not Bloco_0.Gravado then
    WriteBloco_0;

  /// BLOCO C
  WriteRegistroC001;

  if Bloco_C.RegistroC001.IND_DAD = icSemConteudo then
    FechaBloco := True;

  if FechaBloco then
    WriteRegistroC990;

  Bloco_C.WriteBuffer;
  Bloco_C.Conteudo.Clear;
  Bloco_C.Gravado := FechaBloco;

end;

procedure TACBrSEF2.WriteBloco_E;
begin
  if Bloco_E.Gravado then
    Exit;

  /// BLOCO E
  WriteRegistroE001;
  WriteRegistroE990;
  Bloco_E.WriteBuffer;
  Bloco_E.Conteudo.Clear;
  Bloco_E.Gravado := True;
end;

procedure TACBrSEF2.WriteBloco_F;
begin
  WriteRegistroF001;
  WriteRegistroF990;
  Bloco_F.WriteBuffer;
  Bloco_F.Gravado := True;
end;

procedure TACBrSEF2.WriteBloco_H;
begin
  if Bloco_H.Gravado then
    Exit;

  WriteRegistroH001;
  WriteRegistroH990;
  Bloco_H.WriteBuffer;
  Bloco_H.Conteudo.Clear;
  Bloco_H.Gravado := True;
end;

procedure TACBrSEF2.WriteRegistro0000;
begin
  with Bloco_9.Registro9900.New do
  begin
    REG_BLC := '0000';
    QTD_REG_BLC := 1;
  end;

  Bloco_0.WriteRegistro0000;
end;

procedure TACBrSEF2.WriteRegistro0001;
begin
  Bloco_0.WriteRegistro0001;

  with Bloco_9.Registro9900 do
  begin
    with New do
    begin
      REG_BLC := '0001';
      QTD_REG_BLC := 1;
    end;
  end;

  if Bloco_0.Registro0001.IND_MOV = icContConteudo then
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

      with New do
      begin
        REG_BLC := '0030';
        QTD_REG_BLC := 1;
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

      {if Bloco_0.Registro0175Count > 0 then
      begin
         with New do
         begin
            REG_BLC := '0175';
            QTD_REG_BLC := Bloco_0.Registro0175Count;
         end;
      end;}

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

      if Bloco_0.Registro0215Count > 0 then
      begin
         with New do
         begin
            REG_BLC := '0215';
            QTD_REG_BLC := Bloco_0.Registro0215Count;
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
    end;
  end;
end;

procedure TACBrSEF2.WriteRegistro0990;
begin
  with Bloco_9.Registro9900.New do
  begin
    REG_BLC := '0990';
    QTD_REG_BLC := 1;
  end;
  Bloco_0.WriteRegistro0990;
end;

procedure TACBrSEF2.WriteRegistro9001;
begin
  with Bloco_9.Registro9900.New do
  begin
    REG_BLC := '9001';
    QTD_REG_BLC := 1;
  end;
  Bloco_9.WriteRegistro9001;
end;

procedure TACBrSEF2.WriteRegistro9900;
begin
  with Bloco_9.Registro9900 do
  begin
    with New do
    begin
      REG_BLC := '9900';
      QTD_REG_BLC := Count + 2;
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

procedure TACBrSEF2.WriteRegistro9990;
begin
  Bloco_9.WriteRegistro9990;
end;

procedure TACBrSEF2.WriteRegistro9999;
begin
  Bloco_9.Registro9999.QTD_LIN := Bloco_9.Registro9999.QTD_LIN + Bloco_0.Registro0990.QTD_LIN_0 +
    Bloco_C.RegistroC990.QTD_LIN_C +
    Bloco_E.RegistroE990.QTD_LIN_E +
    Bloco_H.RegistroH990.QTD_LIN_H +
    Bloco_9.Registro9990.QTD_LIN_9;
  Bloco_9.WriteRegistro9999;
end;

procedure TACBrSEF2.WriteRegistroC001;
begin
  Bloco_C.WriteRegistroC001;

  with Bloco_9.Registro9900.New do
  begin
    REG_BLC := 'C001';
    QTD_REG_BLC := 1;
  end;

  if Bloco_C.RegistroC001.IND_DAD = icContConteudo then
  begin
    with Bloco_9.Registro9900 do
    begin
      if Bloco_C.RegistroC020Count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'C020';
          QTD_REG_BLC := Bloco_C.RegistroC020Count;
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

      if Bloco_C.RegistroC550Count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'C550';
          QTD_REG_BLC := Bloco_C.RegistroC550Count;
        end;
      end;

      if Bloco_C.RegistroC560Count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'C560';
          QTD_REG_BLC := Bloco_C.RegistroC560Count;
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

      if Bloco_C.RegistroC610Count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'C610';
          QTD_REG_BLC := Bloco_C.RegistroC610Count;
        end;
      end;
    end;
  end;
end;

procedure TACBrSEF2.WriteRegistroC990;
begin
  with Bloco_9.Registro9900.New do
  begin
    REG_BLC := 'C990';
    QTD_REG_BLC := 1;
  end;
  Bloco_C.WriteRegistroC990;
end;

procedure TACBrSEF2.WriteRegistroE001;
begin
  Bloco_E.WriteRegistroE001;

  with Bloco_9.Registro9900.New do
  begin
    REG_BLC := 'E001';
    QTD_REG_BLC := 1;
  end;

  with Bloco_9.Registro9900 do
  begin
    if Bloco_E.RegistroE020Count > 0 then
    begin
      with New do
      begin
        REG_BLC := 'E020';
        QTD_REG_BLC := Bloco_E.RegistroE020Count;
      end;
    end;

    if Bloco_E.RegistroE025Count > 0 then
    begin
      with New do
      begin
        REG_BLC := 'E025';
        QTD_REG_BLC := Bloco_E.RegistroE025Count;
      end;
    end;

    if Bloco_E.RegistroE050Count > 0 then
    begin
      with New do
      begin
        REG_BLC := 'E050';
        QTD_REG_BLC := Bloco_E.RegistroE050Count;
      end;
    end;

    if Bloco_E.RegistroE055Count > 0 then
    begin
      with New do
      begin
        REG_BLC := 'E055';
        QTD_REG_BLC := Bloco_E.RegistroE055Count;
      end;
    end;

    if Bloco_E.RegistroE060Count > 0 then
    begin
      with New do
      begin
        REG_BLC := 'E060';
        QTD_REG_BLC := Bloco_E.RegistroE060Count;
      end;
    end;

    if Bloco_E.RegistroE060Count > 0 then
    begin
      with New do
      begin
        REG_BLC := 'E065';
        QTD_REG_BLC := Bloco_E.RegistroE065Count;
      end;
    end;

    if Bloco_E.RegistroE080Count > 0 then
    begin
      with New do
      begin
        REG_BLC := 'E080';
        QTD_REG_BLC := Bloco_E.RegistroE080Count;
      end;
    end;

    if Bloco_E.RegistroE085Count > 0 then
    begin
      with New do
      begin
        REG_BLC := 'E085';
        QTD_REG_BLC := Bloco_E.RegistroE085Count;
      end;
    end;

    if Bloco_E.RegistroE100Count > 0 then
    begin
      with New do
      begin
        REG_BLC := 'E100';
        QTD_REG_BLC := Bloco_E.RegistroE100Count;
      end;
    end;

    if Bloco_E.RegistroE105Count > 0 then
    begin
      with New do
      begin
        REG_BLC := 'E105';
        QTD_REG_BLC := Bloco_E.RegistroE105Count;
      end;
    end;

    if Bloco_E.RegistroE120Count > 0 then
    begin
      with New do
      begin
        REG_BLC := 'E120';
        QTD_REG_BLC := Bloco_E.RegistroE120Count;
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
    if Bloco_E.RegistroE305Count > 0 then
    begin
      with New do
      begin
        REG_BLC := 'E305';
        QTD_REG_BLC := Bloco_E.RegistroE305Count;
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
    if Bloco_E.RegistroE330Count > 0 then
    begin
      with New do
      begin
        REG_BLC := 'E330';
        QTD_REG_BLC := Bloco_E.RegistroE330Count;
      end;
    end;
    if Bloco_E.RegistroE340Count > 0 then
    begin
      with New do
      begin
        REG_BLC := 'E340';
        QTD_REG_BLC := Bloco_E.RegistroE340Count;
      end;
    end;
    if Bloco_E.RegistroE350Count > 0 then
    begin
      with New do
      begin
        REG_BLC := 'E350';
        QTD_REG_BLC := Bloco_E.RegistroE350Count;
      end;
    end;
    if Bloco_E.RegistroE360Count > 0 then
    begin
      with New do
      begin
        REG_BLC := 'E360';
        QTD_REG_BLC := Bloco_E.RegistroE360Count;
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
    if Bloco_E.RegistroE520Count > 0 then
    begin
      with New do
      begin
        REG_BLC := 'E520';
        QTD_REG_BLC := Bloco_E.RegistroE520Count;
      end;
    end;
    if Bloco_E.RegistroE525Count > 0 then
    begin
      with New do
      begin
        REG_BLC := 'E525';
        QTD_REG_BLC := Bloco_E.RegistroE525Count;
      end;
    end;
    if Bloco_E.RegistroE540Count > 0 then
    begin
      with New do
      begin
        REG_BLC := 'E540';
        QTD_REG_BLC := Bloco_E.RegistroE540Count;
      end;
    end;
    if Bloco_E.RegistroE550Count > 0 then
    begin
      with New do
      begin
        REG_BLC := 'E550';
        QTD_REG_BLC := Bloco_E.RegistroE550Count;
      end;
    end;


    if Bloco_E.RegistroE560Count > 0 then
    begin
      with New do
      begin
        REG_BLC := 'E560';
        QTD_REG_BLC := Bloco_E.RegistroE560Count;
      end;
    end;
  end;
end;

procedure TACBrSEF2.WriteRegistroE990;
begin
  with Bloco_9.Registro9900.New do
  begin
    REG_BLC := 'E990';
    QTD_REG_BLC := 1;
  end;
  Bloco_E.WriteRegistroE990;
end;

procedure TACBrSEF2.WriteRegistroF001;
begin
  Bloco_F.WriteRegistroF001;

  with Bloco_9.Registro9900.New do
  begin
    REG_BLC := 'F001';
    QTD_REG_BLC := 1;
  end;

  with Bloco_9.Registro9900 do
  begin
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

    if Bloco_F.RegistroF215Count > 0 then
    begin
      with New do
      begin
        REG_BLC := 'F215';
        QTD_REG_BLC := Bloco_F.RegistroF215Count;
      end;
    end;

    if Bloco_F.RegistroF220Count > 0 then
    begin
      with New do
      begin
        REG_BLC := 'F220';
        QTD_REG_BLC := Bloco_F.RegistroF220Count;
      end;
    end;

    if Bloco_F.RegistroF230Count > 0 then
    begin
      with New do
      begin
        REG_BLC := 'F230';
        QTD_REG_BLC := Bloco_F.RegistroF230Count;
      end;
    end;
  end;
end;

procedure TACBrSEF2.WriteRegistroF990;
begin
  with Bloco_9.Registro9900.New do
  begin
    REG_BLC := 'F990';
    QTD_REG_BLC := 1;
  end;
  Bloco_F.WriteRegistroF990;
end;

procedure TACBrSEF2.WriteRegistroH001;
begin
  Bloco_H.WriteRegistroH001;
  with Bloco_9.Registro9900.New do
  begin
    REG_BLC := 'H001';
    QTD_REG_BLC := 1;
  end;

  if Bloco_H.RegistroH001.IND_DAD = icContConteudo then
  begin
    with Bloco_9.Registro9900 do
    begin
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

      if Bloco_H.RegistroH040Count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'H040';
          QTD_REG_BLC := Bloco_H.RegistroH040Count;
        end;
      end;

      if Bloco_H.RegistroH050Count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'H050';
          QTD_REG_BLC := Bloco_H.RegistroH050Count;
        end;
      end;

      if Bloco_H.RegistroH060Count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'H060';
          QTD_REG_BLC := Bloco_H.RegistroH060Count;
        end;
      end;
    end;
  end;
end;

procedure TACBrSEF2.WriteRegistroH990;
begin
  with Bloco_9.Registro9900.New do
  begin
    REG_BLC := 'H990';
    QTD_REG_BLC := 1;
  end;
  Bloco_H.WriteRegistroH990;
end;

{$IFDEF FPC}
initialization
	{$i ACBrSEF2.lrs}
{$ENDIF}

end.
