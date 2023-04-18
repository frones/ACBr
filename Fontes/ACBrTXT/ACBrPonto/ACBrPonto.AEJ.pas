{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Alisson Souza Pereira                           }
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
unit ACBrPonto.AEJ;

interface

uses
  SysUtils,
  Classes,
  ACBrPonto.Conversao,
  ACBrTXTClass,
  Contnrs,
  DateUtils;

type
  TCabecalho = class(TACBrTXTClass)
  private
    FtipoReg: String;
    FtpIdtEmpregador: TtpIdtEmpregador;
    FidtEmpregador: String;
    Fcaepf: String;
    Fcno: String;
    FrazaoOuNome: String;
    FdataInicialAej: TDateTime;
    FdataFinalAej: TDateTime;
    FdataHoraGerAej: TDateTime;
    FversaoAej: String;
  public
    constructor Create;
    function GetStr: String;

    property tipoReg        : String           read FtipoReg;
    property tpIdtEmpregador: TtpIdtEmpregador read FtpIdtEmpregador write FtpIdtEmpregador;
    property idtEmpregador  : String           read FidtEmpregador   write FidtEmpregador;
    property caepf          : String           read Fcaepf           write Fcaepf;
    property cno            : String           read Fcno             write Fcno;
    property razaoOuNome    : String           read FrazaoOuNome     write FrazaoOuNome;
    property dataInicialAej : TDateTime        read FdataInicialAej  write FdataInicialAej;
    property dataFinalAej   : TDateTime        read FdataFinalAej    write FdataFinalAej;
    property dataHoraGerAej : TDateTime        read FdataHoraGerAej  write FdataHoraGerAej;
    property versaoAej      : String           read FversaoAej;
  end;

  TCabecalhoList = class(TObjectList)
  private
    function GetItem(Index: Integer): TCabecalho;
    procedure SetItem(Index: Integer; const Value: TCabecalho);
  public
    function New: TCabecalho;
    property Items[Index: Integer]: TCabecalho read GetItem write SetItem;
    function GetStr: String;
  end;

  TRegistro02 = class(TACBrTXTClass)
  private
    FtipoReg: String;
    FidRepAej: String;
    FtpRep: TtpRep;
    FnrRep: String;
  public
    constructor Create;
    function GetStr: String;

    property tipoReg: String read FtipoReg;
    property idRepAej: String read FidRepAej write FidRepAej;
    property tpRep: TtpRep read FtpRep write FtpRep;
    property nrRep: String read FnrRep write FnrRep;
  end;

  TRegistro02List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistro02;
    procedure SetItem(Index: Integer; const Value: TRegistro02);
  public
    function New: TRegistro02;
    property Items[Index: Integer]: TRegistro02 read GetItem write SetItem;
    function GetStr: String;
  end;

  TRegistro03 = class(TACBrTXTClass)
  private
    FtipoReg: String;
    FidtVinculoAej: String;
    Fcpf: String;
    FnomeEmp: String;
  public
    constructor Create;
    function GetStr: String;

    property tipoReg: String read FtipoReg;
    property idtVinculoAej: String read FidtVinculoAej write FidtVinculoAej;
    property cpf: String read Fcpf write Fcpf;
    property nomeEmp: String read FnomeEmp write FnomeEmp;
  end;

  TRegistro03List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistro03;
    procedure SetItem(Index: Integer; const Value: TRegistro03);
  public
    function New: TRegistro03;
    property Items[Index: Integer]: TRegistro03 read GetItem write SetItem;
    function GetStr: String;
  end;

  TRegistro04 = class(TACBrTXTClass)
  private
    FtipoReg: String;
    FcodHorContratual: String;
    FdurJornada: Integer;
    FhrEntrada01: String;
    FhrSaida01: String;
    FhrEntrada02: String;
    FhrSaida02: String;
  public
    constructor Create;
    function GetStr: String;

    property tipoReg: String read FtipoReg;
    property codHorContratual: String read FcodHorContratual write FcodHorContratual;
    property durJornada: Integer read FdurJornada write FdurJornada;
    property hrEntrada01: String read FhrEntrada01 write FhrEntrada01;
    property hrSaida01: String read FhrSaida01 write FhrSaida01;
    property hrEntrada02: String read FhrEntrada02 write FhrEntrada02;
    property hrSaida02: String read FhrSaida02 write FhrSaida02;
  end;

  TRegistro04List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistro04;
    procedure SetItem(Index: Integer; const Value: TRegistro04);
  public
    function New: TRegistro04;
    property Items[Index: Integer]: TRegistro04 read GetItem write SetItem;
    function GetStr: String;
  end;

  TRegistro05 = class(TACBrTXTClass)
  private
    FtipoReg: String;
    FidtVinculoAej: String;
    FdataHoraMarc: TDateTime;
    FidRepAej: String;
    FtpMarc: TtpMarc;
    FseqEntSaida: Integer;
    FfonteMarc: TfonteMarc;
    FcodHorContratual: String;
    Fmotivo: String;
  public
    constructor Create;
    function GetStr: String;

    property tipoReg: String read FtipoReg;
    property idtVinculoAej: String read FidtVinculoAej write FidtVinculoAej;
    property dataHoraMarc: TDateTime read FdataHoraMarc write FdataHoraMarc;
    property idRepAej: String read FidRepAej write FidRepAej;
    property tpMarc: TtpMarc read FtpMarc write FtpMarc;
    property seqEntSaida: Integer read FseqEntSaida write FseqEntSaida;
    property fonteMarc: TfonteMarc read FfonteMarc write FfonteMarc;
    property codHorContratual: String read FcodHorContratual write FcodHorContratual;
    property motivo: String read Fmotivo write Fmotivo;
  end;

  TRegistro05List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistro05;
    procedure SetItem(Index: Integer; const Value: TRegistro05);
  public
    function New: TRegistro05;
    property Items[Index: Integer]: TRegistro05 read GetItem write SetItem;
    function GetStr: String;
  end;

  TRegistro06 = class(TACBrTXTClass)
  private
    FtipoReg: String;
    FidtVinculoAej: String;
    FmatEsocial: String;
  public
    constructor Create;
    function GetStr: String;

    property tipoReg: String read FtipoReg;
    property idtVinculoAej: String read FidtVinculoAej write FidtVinculoAej;
    property matEsocial: String read FmatEsocial write FmatEsocial;
  end;

  TRegistro06List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistro06;
    procedure SetItem(Index: Integer; const Value: TRegistro06);
  public
    function New: TRegistro06;
    property Items[Index: Integer]: TRegistro06 read GetItem write SetItem;
    function GetStr: String;
  end;

  TRegistro07 = class(TACBrTXTClass)
  private
    FtipoReg: String;
    FidtVinculoAej: String;
    FtipoAusenOuComp: TtipoAusenOuComp;
    Fdata: TDateTime;
    FqtMinutos: String;
    FtipoMovBH: String;
  public
    constructor Create;
    function GetStr: String;

    property tipoReg: String read FtipoReg;
    property idtVinculoAej: String read FidtVinculoAej write FidtVinculoAej;
    property tipoAusenOuComp: TtipoAusenOuComp read FtipoAusenOuComp write FtipoAusenOuComp;
    property data: TDateTime read Fdata write Fdata;
    property qtMinutos: String read FqtMinutos write FqtMinutos;
    property tipoMovBH: String read FtipoMovBH write FtipoMovBH;
  end;

  TRegistro07List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistro07;
    procedure SetItem(Index: Integer; const Value: TRegistro07);
  public
    function New: TRegistro07;
    property Items[Index: Integer]: TRegistro07 read GetItem write SetItem;
    function GetStr: String;
  end;

  TRegistro08 = class(TACBrTXTClass)
  private
    FtipoReg: String;
    FnomeProg: String;
    FversaoProg: String;
    FtpIdtDesenv: TtpIdtDesenv;
    FidtDesenv: String;
    FrazaoNomeDesenv: String;
    FemailDesenv: String;
  public
    constructor Create;
    function GetStr: String;

    property tipoReg        : String  read FtipoReg;
    property nomeProg       : String  read FnomeProg      write FnomeProg;
    property versaoProg     : String  read FversaoProg    write FversaoProg;
    property tpIdtDesenv    : TtpIdtDesenv  read FtpIdtDesenv   write FtpIdtDesenv;
    property idtDesenv      : String  read FidtDesenv     write FidtDesenv;
    property razaoNomeDesenv: String  read FrazaoNomeDesenv write FrazaoNomeDesenv;
    property emailDesenv    : String  read FemailDesenv   write FemailDesenv;
  end;

  TRegistro08List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistro08;
    procedure SetItem(Index: Integer; const Value: TRegistro08);
  public
    function New: TRegistro08;
    property Items[Index: Integer]: TRegistro08 read GetItem write SetItem;
    function GetStr: String;
  end;

  TAssinaturaDigital = class(TACBrTXTClass)
  private
    FassinDigital: String;
  public
    constructor Create;
    function GetStr: String;

    property assinDigital : String  read FassinDigital;
  end;

  // Registro Trailer
  TTrailer =  class(TACBrTXTClass)
  private
    FtipoReg: String;
    FCabecalho : TCabecalhoList;
    FRegistro02: TRegistro02List;
    FRegistro03: TRegistro03List;
    FRegistro04: TRegistro04List;
    FRegistro05: TRegistro05List;
    FRegistro06: TRegistro06List;
    FRegistro07: TRegistro07List;
    FRegistro08: TRegistro08List;
  public
    constructor Create(
      oReg01: TCabecalhoList;
      oReg02: TRegistro02List;
      oReg03: TRegistro03List;
      oReg04: TRegistro04List;
      oReg05: TRegistro05List;
      oReg06: TRegistro06List;
      oReg07: TRegistro07List;
      oReg08: TRegistro08List
    );

    function GetStr: String;

    property tipoReg: String read FtipoReg;
  end;

  TAEJ = class(TACBrTXTClass)
  private
    FCabecalho  : TCabecalhoList;
    FRegistro02 : TRegistro02List;
    FRegistro03 : TRegistro03List;
    FRegistro04 : TRegistro04List;
    FRegistro05 : TRegistro05List;
    FRegistro06 : TRegistro06List;
    FRegistro07 : TRegistro07List;
    FRegistro08 : TRegistro08List;
    FTrailer    : TTrailer;
    FAssinaturaDigital : TAssinaturaDigital;

    procedure CriaRegistros;
    procedure LiberaRegistros;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LimpaRegistros;

    property Cabecalho : TCabecalhoList  read FCabecalho write FCabecalho;
    property Registro02: TRegistro02List read FRegistro02 write FRegistro02;
    property Registro03: TRegistro03List read FRegistro03 write FRegistro03;
    property Registro04: TRegistro04List read FRegistro04 write FRegistro04;
    property Registro05: TRegistro05List read FRegistro05 write FRegistro05;
    property Registro06: TRegistro06List read FRegistro06 write FRegistro06;
    property Registro07: TRegistro07List read FRegistro07 write FRegistro07;
    property Registro08: TRegistro08List read FRegistro08 write FRegistro08;
    property Trailer: TTrailer           read FTrailer write FTrailer;
    property AssinaturaDigital: TAssinaturaDigital  read FAssinaturaDigital write FAssinaturaDigital;
  end;

implementation

uses
  ACBrUtil.DateTime;

{ TCabecalho }

constructor TCabecalho.Create;
begin
  FtipoReg   := '01';
  FversaoAej := '001';
end;

function TCabecalho.GetStr: String;
begin
  Result :=
    LFill(tipoReg) +
    LFill(tpIdtEmpregadorToStr(tpIdtEmpregador)) +
    LFill(idtEmpregador, 14) +
    LFill(caepf, 14) +
    LFill(cno, 12) +
    RFill(razaoOuNome, 150) +
    LFill(FormatDateTime('yyyy-mm-dd', dataInicialAej ), 10) +
    LFill(FormatDateTime('yyyy-mm-dd', dataFinalAej), 10) +
    LFill(DateTimeToIso8601(dataHoraGerAej), 24) +
    LFill(versaoAej) +
    sLineBreak;
end;

{ TCabecalhoList }

function TCabecalhoList.GetItem(Index: Integer): TCabecalho;
begin
  Result := TCabecalho(inherited GetItem(Index));
end;

function TCabecalhoList.GetStr: String;
var
  i: Integer;
begin
  for i := 0 to Self.Count - 1 do
    Result := Result + Self.Items[i].GetStr;
end;

function TCabecalhoList.New: TCabecalho;
begin
  Result := TCabecalho.Create;
  Add(Result);
end;

procedure TCabecalhoList.SetItem(Index: Integer; const Value: TCabecalho);
begin
  Put(Index, Value);
end;

{ TRegistro2 }

constructor TRegistro02.Create;
begin
  FtipoReg := '02';
end;

function TRegistro02.GetStr: String;
begin
  Result :=
    LFill(tipoReg) +
    LFill(idRepAej,9) +
    LFill(tpRepToStr(tpRep)) +
    LFill(nrRep, 17) +
    sLineBreak;
end;

{ TRegistro02List }

function TRegistro02List.GetStr: String;
var
  i: Integer;
begin
  for i := 0 to Self.Count - 1 do
    Result := Result + Self.Items[i].GetStr;
end;

function TRegistro02List.GetItem(Index: Integer): TRegistro02;
begin
  Result := TRegistro02(inherited GetItem(Index));
end;

function TRegistro02List.New: TRegistro02;
begin
  Result := TRegistro02.Create;
  Add(Result);
end;

procedure TRegistro02List.SetItem(Index: Integer; const Value: TRegistro02);
begin
  Put(Index, Value);
end;

{ TRegistro03 }

constructor TRegistro03.Create;
begin
  FtipoReg := '03';
end;

function TRegistro03.GetStr: String;
begin
  Result :=
    LFill(tipoReg) +
    LFill(idtVinculoAej,9) +
    LFill(cpf,11) +
    RFill(nomeEmp, 150) +
    sLineBreak;
end;

{ TRegistro03List }

function TRegistro03List.GetItem(Index: Integer): TRegistro03;
begin
  Result := TRegistro03(inherited GetItem(Index));
end;

function TRegistro03List.GetStr: String;
var
  i: Integer;
begin
  for i := 0 to Self.Count - 1 do
    Result := Result + Self.Items[i].GetStr;
end;

function TRegistro03List.New: TRegistro03;
begin
  Result := TRegistro03.Create;
  Add(Result);
end;

procedure TRegistro03List.SetItem(Index: Integer; const Value: TRegistro03);
begin
  Put(Index, Value);
end;

{ TRegistro04 }

constructor TRegistro04.Create;
begin
  FtipoReg := '04';
end;

function TRegistro04.GetStr: String;
begin
  Result :=
    LFill(tipoReg) +
    RFill(codHorContratual,30) +
    RFill(IntToStr(durJornada),12) +
    LFill(hrEntrada01, 4) +
    LFill(hrSaida01, 4) +
    RFill(hrEntrada02, 4) +
    RFill(hrSaida02, 4) +
    sLineBreak;
end;

{ TRegistro04List }

function TRegistro04List.GetItem(Index: Integer): TRegistro04;
begin
  Result := TRegistro04(inherited GetItem(Index));
end;

function TRegistro04List.GetStr: String;
var
  i: Integer;
begin
  for i := 0 to Self.Count - 1 do
    Result := Result + Self.Items[i].GetStr;
end;

function TRegistro04List.New: TRegistro04;
begin
  Result := TRegistro04.Create;
  Add(Result);
end;

procedure TRegistro04List.SetItem(Index: Integer; const Value: TRegistro04);
begin
  Put(Index, Value);
end;

{ TRegistro05 }

constructor TRegistro05.Create;
begin
  FtipoReg := '05';
end;

function TRegistro05.GetStr: String;
begin
  Result :=
    LFill(tipoReg) +
    RFill(idtVinculoAej,9) +
    LFill(DateTimeToIso8601(dataHoraMarc),24) +
    RFill(idRepAej, 9) +
    LFill(tpMarcToStr(tpMarc)) +
    LFill(IntToStr(seqEntSaida), 3) +
    LFill(fonteMarcToStr(fonteMarc)) +
    RFill(codHorContratual, 30) +
    RFill(motivo, 150) +
    sLineBreak;
end;

{ TRegistro05List }

function TRegistro05List.GetItem(Index: Integer): TRegistro05;
begin
  Result := TRegistro05(inherited GetItem(Index));
end;

function TRegistro05List.GetStr: String;
var
  i: Integer;
begin
  for i := 0 to Self.Count - 1 do
    Result := Result + Self.Items[i].GetStr;
end;

function TRegistro05List.New: TRegistro05;
begin
  Result := TRegistro05.Create;
  Add(Result);
end;

procedure TRegistro05List.SetItem(Index: Integer; const Value: TRegistro05);
begin
  Put(Index, Value);
end;

{ TRegistro06 }

constructor TRegistro06.Create;
begin
  FtipoReg := '06';
end;

function TRegistro06.GetStr: String;
begin
  Result :=
    LFill(tipoReg) +
    RFill(idtVinculoAej,9) +
    RFill(matEsocial,30) +
    sLineBreak;
end;

{ TRegistro06List }

function TRegistro06List.GetItem(Index: Integer): TRegistro06;
begin
  Result := TRegistro06(inherited GetItem(Index));
end;

function TRegistro06List.GetStr: String;
var
  i: Integer;
begin
  for i := 0 to Self.Count - 1 do
    Result := Result + Self.Items[i].GetStr;
end;

function TRegistro06List.New: TRegistro06;
begin
  Result := TRegistro06.Create;
  Add(Result);
end;

procedure TRegistro06List.SetItem(Index: Integer; const Value: TRegistro06);
begin
  Put(Index, Value);
end;

{ TRegistro07 }

constructor TRegistro07.Create;
begin
  FtipoReg := '07';
end;

function TRegistro07.GetStr: String;
begin
  Result :=
    LFill(tipoReg) +
    RFill(idtVinculoAej,9) +
    LFill(tipoAusenOuCompToStr(tipoAusenOuComp)) +
    LFill(FormatDateTime('yyyy-mm-dd', data),10) +
    RFill(qtMinutos,12) +
    RFill(tipoMovBH,9) +
    sLineBreak;
end;

{ TRegistro07List }

function TRegistro07List.GetItem(Index: Integer): TRegistro07;
begin
  Result := TRegistro07(inherited GetItem(Index));
end;

function TRegistro07List.GetStr: String;
var
  i: Integer;
begin
  for i := 0 to Self.Count - 1 do
    Result := Result + Self.Items[i].GetStr;
end;

function TRegistro07List.New: TRegistro07;
begin
  Result := TRegistro07.Create;
  Add(Result);
end;

procedure TRegistro07List.SetItem(Index: Integer; const Value: TRegistro07);
begin
  Put(Index, Value);
end;

{ TRegistro08 }

constructor TRegistro08.Create;
begin
  FtipoReg := '08';
end;

function TRegistro08.GetStr: String;
begin
  Result :=
    LFill(tipoReg) +
    RFill(nomeProg,150) +
    RFill(versaoProg,8) +
    LFill(tpIdtDesenvToStr(tpIdtDesenv)) +
    LFill(idtDesenv,14) +
    RFill(razaoNomeDesenv,150) +
    RFill(emailDesenv,50) +
    sLineBreak;
end;

{ TRegistro08List }

function TRegistro08List.GetItem(Index: Integer): TRegistro08;
begin
  Result := TRegistro08(inherited GetItem(Index));
end;

function TRegistro08List.GetStr: String;
var
  i: Integer;
begin
  for i := 0 to Self.Count - 1 do
    Result := Result + Self.Items[i].GetStr;
end;

function TRegistro08List.New: TRegistro08;
begin
  Result := TRegistro08.Create;
  Add(Result);
end;

procedure TRegistro08List.SetItem(Index: Integer; const Value: TRegistro08);
begin
  Put(Index, Value);
end;

{ TTrailer }

constructor TTrailer.Create(
      oReg01: TCabecalhoList;
      oReg02: TRegistro02List;
      oReg03: TRegistro03List;
      oReg04: TRegistro04List;
      oReg05: TRegistro05List;
      oReg06: TRegistro06List;
      oReg07: TRegistro07List;
      oReg08: TRegistro08List
    );
begin
  FtipoReg    := '99';
  FCabecalho  := oReg01;
  FRegistro02 := oReg02;
  FRegistro03 := oReg03;
  FRegistro04 := oReg04;
  FRegistro05 := oReg05;
  FRegistro06 := oReg06;
  FRegistro07 := oReg07;
  FRegistro08 := oReg08;
end;

function TTrailer.GetStr: String;
begin
  Result :=
    LFill(tipoReg) +
    RFill(IntToStr(FCabecalho.Count),9) +
    RFill(IntToStr(FRegistro02.Count),9) +
    RFill(IntToStr(FRegistro03.Count),9) +
    RFill(IntToStr(FRegistro04.Count),9) +
    RFill(IntToStr(FRegistro05.Count),9) +
    RFill(IntToStr(FRegistro06.Count),9) +
    RFill(IntToStr(FRegistro07.Count),9) +
    RFill(IntToStr(FRegistro08.Count),9) +
    sLineBreak;
end;

{ TAEJ }

constructor TAEJ.Create;
begin
  inherited Create;
  CriaRegistros;
end;

procedure TAEJ.CriaRegistros;
begin
  FCabecalho  := TCabecalhoList.Create;
  FRegistro02 := TRegistro02List.Create;
  FRegistro03 := TRegistro03List.Create;
  FRegistro04 := TRegistro04List.Create;
  FRegistro05 := TRegistro05List.Create;
  FRegistro06 := TRegistro06List.Create;
  FRegistro07 := TRegistro07List.Create;
  FRegistro08 := TRegistro08List.Create;

  FTrailer := TTrailer.Create(
    FCabecalho,
    FRegistro02,
    FRegistro03,
    FRegistro04,
    FRegistro05,
    FRegistro06,
    FRegistro07,
    FRegistro08
  );

  FAssinaturaDigital := TAssinaturaDigital.Create;
end;

destructor TAEJ.Destroy;
begin
  LiberaRegistros;
  inherited;
end;

procedure TAEJ.LiberaRegistros;
begin
  FCabecalho.Free;
  FRegistro02.Free;
  FRegistro03.Free;
  FRegistro04.Free;
  FRegistro05.Free;
  FRegistro06.Free;
  FRegistro07.Free;
  FRegistro08.Free;
  FTrailer.Free;
  FAssinaturaDigital.Free;
end;

procedure TAEJ.LimpaRegistros;
begin
  LiberaRegistros;
  CriaRegistros;
end;

{ TAssinaturaDigital }

constructor TAssinaturaDigital.Create;
begin
  FassinDigital := 'ASSINATURA_DIGITAL_EM_ARQUIVO_P7S';
end;

function TAssinaturaDigital.GetStr: String;
begin
  Result :=
    RFill(FassinDigital,100) +
    sLineBreak;
end;

end.
