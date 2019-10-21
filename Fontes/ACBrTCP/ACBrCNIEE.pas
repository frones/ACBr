{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2004 Daniel Simoes de Almeida               }
{                                       Régys Silveira                         }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr    }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }
{                                                                              }
{ Esse arquivo usa a classe  SynaSer   Copyright (c)2001-2003, Lukas Gebauer   }
{  Project : Ararat Synapse     (Found at URL: http://www.ararat.cz/synapse/)  }
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
|* 24/10/2008: Primeira Versao
|*    Daniel Simoes de Almeida
******************************************************************************}

{$I ACBr.inc}

unit ACBrCNIEE;

interface

uses
  Contnrs,  SysUtils, Variants, Classes,
  ACBrUtil, ACBrSocket;

const
  CURL_Download_Tabela_CNIEE = 'http://www.fazenda.mg.gov.br/empresas/ecf/files/Tabela_CNIEE.bin' ;

type
  EACBrCNIEE = class(Exception);

  TACBrCNIEEExporta = (exTXT, exCSV, exDSV, exXML, exHTML);

  TRegistro = packed record
    Marca        : string[2];
    Modelo       : string[2];
    Versao       : string[2];
    Tipo         : string[10];
    MarcaDescr   : string[30];
    ModeloDescr  : string[30];
    VersaoSB     : string[20];
    QtLacreSL    : Integer;
    QTLacreFab   : Integer;
    MFD          : string[3];
    LacreMFD     : string[3];
    AtoAprovacao : string[25];
    AtoRegistroMG: string[25];
    FormatoNumero: string[20];
  end;

  TACBrCNIEERegistro = class
  private
    FDescrModelo: String;
    FCodModelo: String;
    FAtoAprovacao: String;
    FVersao: String;
    FQtLacresFab: Integer;
    FQtLacresSL: Integer;
    FTemLacreMFD: String;
    FDescrMarca: String;
    FAtoRegistro: String;
    FTemMFD: String;
    FCodMarca: String;
    FCodVersao: String;
    FFormatoNumFabricacao: String;
    FTipoECF: String;
  public
    property CodMarca: String read FCodMarca write FCodMarca;
    property CodModelo: String read FCodModelo write FCodModelo;
    property CodVersao: String read FCodVersao write FCodVersao;
    property TipoECF: String read FTipoECF write FTipoECF;
    property DescrMarca: String read FDescrMarca write FDescrMarca;
    property DescrModelo: String read FDescrModelo write FDescrModelo;
    property Versao: String read FVersao write FVersao;
    property TemMFD: String read FTemMFD write FTemMFD;
    property TemLacreMFD: String read FTemLacreMFD write FTemLacreMFD;
    property AtoAprovacao: String read FAtoAprovacao write FAtoAprovacao;
    property AtoRegistro: String read FAtoRegistro write FAtoRegistro;
    property FormatoNumFabricacao: String read FFormatoNumFabricacao write FFormatoNumFabricacao;
    property QtLacresSL: Integer read FQtLacresSL write FQtLacresSL;
    property QtLacresFab: Integer read FQtLacresFab write FQtLacresFab;
  end;

  TACBrCNIEERegistros = class(TObjectList)
  private
    function GetItem(Index: integer): TACBrCNIEERegistro;
    procedure SetItem(Index: integer; const Value: TACBrCNIEERegistro);
  public
    function New: TACBrCNIEERegistro;
    property Items[Index: integer]: TACBrCNIEERegistro read GetItem write SetItem; default;
  end;

  { TACBrCNIEE }
	{$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32 or
  pidiOSSimulator or  pidAndroid or
  pidLinux32 or pidiOSDevice
  {$IFDEF RTL300_UP}
  or pidiOSDevice32 or pidLinux64
  or pidWinNX32 or pidWinIoT32
  or pidiOSDevice64
  or pidOSX64 or pidLinux32Arm
  or pidLinux64Arm or pidAndroid64Arm
  {$ENDIF RTL300_UP})]
  {$ENDIF RTL230_UP}
TACBrCNIEE = class(TACBrHTTP)
  private
    FArquivo: String;
    FVersaoArquivo: String;
    FURLDownload: String;
    FCadastros: TACBrCNIEERegistros;
    procedure ExportarCSV(const AArquivo: String);
    procedure ExportarDSV(const AArquivo: String);
    procedure ExportarHTML(const AArquivo: String);
    procedure ExportarXML(const AArquivo: String);
    procedure ExportarTXT(const AArquivo: String);
  public
    destructor Destroy; override;
    constructor Create(AOwner: TComponent); override;

    function DownloadTabela: Boolean;
    function AbrirTabela: Boolean;
    procedure Exportar(const AArquivo: String; ATipo: TACBrCNIEEExporta); overload;
    procedure Exportar(const AArquivo, ADelimitador: String); overload;
    function BuscarECF(const AMarca, AModelo, AVersaoSB: String;
      const RemoveEspacos: Boolean = False): TACBrCNIEERegistro;

    property Cadastros: TACBrCNIEERegistros read FCadastros;

  published
    property Arquivo: String read FArquivo write FArquivo;
    property VersaoArquivo : String read FVersaoArquivo ;
    property URLDownload: String read FURLDownload write FURLDownload;
  end;

implementation

uses
  StrUtils;

{ TACBrCNIEERegistros }

function TACBrCNIEERegistros.GetItem(Index: integer): TACBrCNIEERegistro;
begin
  Result := TACBrCNIEERegistro(inherited Items[Index]);
end;

function TACBrCNIEERegistros.New: TACBrCNIEERegistro;
begin
  Result := TACBrCNIEERegistro.Create;
  Add(Result);
end;

procedure TACBrCNIEERegistros.SetItem(Index: integer;
  const Value: TACBrCNIEERegistro);
begin
  Put(Index, Value);
end;

{ TACBrCNIEE }

constructor TACBrCNIEE.Create(AOwner: TComponent);
begin
  inherited;

  FCadastros     := TACBrCNIEERegistros.Create( True );
  FURLDownload   := CURL_Download_Tabela_CNIEE;
  FArquivo       := '';
  FVersaoArquivo := '';
end;

destructor TACBrCNIEE.Destroy;
begin
  FCadastros.Free;
  inherited;
end;


function TACBrCNIEE.DownloadTabela: Boolean;
Var
  URLVersao, ArquivoVersao : String ;
begin
  if Trim(FURLDownload) = '' then
    raise EACBrCNIEE.Create( ACBrStr('URL de Download não informada.') );

  if Trim(FArquivo) = '' then
    raise EACBrCNIEE.Create( ACBrStr('Nome do arquivo em disco não especificado.') );

  try
     HTTPGet( FURLDownload );
     HTTPSend.Document.Seek(0, soFromBeginning);
     HTTPSend.Document.SaveToFile( FArquivo );
     Result := True;

     // Baixando o arquivo de Versao //
     try
        URLVersao     := StringReplace( FURLDownload, 'Tabela_CNIEE.bin', 'versao.txt', [] ) ;
        ArquivoVersao := ExtractFilePath( FArquivo ) + 'versao.txt';
        HTTPGet( URLVersao );
        HTTPSend.Document.Seek(0, soFromBeginning);
        HTTPSend.Document.SaveToFile( ArquivoVersao );
     except
     end ;
  except
     Result := False ;
  end ;
end;

function TACBrCNIEE.AbrirTabela: Boolean;
var
  F: file of TRegistro;
  Registro: TRegistro;
  FileName: String;
  SL : TStringList ;
  I, P : Integer ;
begin
  FileName := Trim(FArquivo);

  if FileName = '' then
    raise Exception.Create( ACBrStr('Nome do arquivo em Disco não especificado.') );

  if not FileExists(FileName) then
    raise Exception.Create( ACBrStr('Arquivo não encontrado:' + sLineBreak + FileName) );

  FCadastros.Clear;
  AssignFile(F, Filename);
  try
    Reset(F);
    while not Eof(F) do
    begin
      Read(F, Registro);

      with FCadastros.New do
      begin
        CodMarca             := Trim(string(Registro.Marca));
        CodModelo            := Trim(string(Registro.Modelo));
        CodVersao            := Trim(string(Registro.Versao));
        TipoECF              := Trim(string(Registro.Tipo));
        DescrMarca           := Trim(string(Registro.MarcaDescr));
        DescrModelo          := Trim(string(Registro.ModeloDescr));
        Versao               := Trim(string(Registro.VersaoSB));
        QtLacresSL           := Registro.QtLacreSL;
        QtLacresFab          := Registro.QTLacreFab;
        TemMFD               := Trim(string(Registro.MFD));
        TemLacreMFD          := Trim(string(Registro.LacreMFD));
        AtoAprovacao         := Trim(string(Registro.AtoAprovacao));
        AtoRegistro          := Trim(string(Registro.AtoRegistroMG));
        FormatoNumFabricacao := Trim(string(Registro.FormatoNumero));
      end;
    end;
    Result := True;

    FileName := ExtractFilePath( FileName ) + 'versao.txt';
    if FileExists( FileName ) then
    begin
       SL := TStringList.Create;
       try
         SL.LoadFromFile( FileName );
         I := 0 ;
         while (FVersaoArquivo = '') and (I < SL.Count) do
         begin
           P := pos( 'Tabela_CNIEE.bin', SL[I] ) ;
           if P > 0 then
              FVersaoArquivo := Trim( copy( SL[1], 1, P-1) ) ;
           Inc( I ) ;
         end ;
       finally
         SL.Free ;
       end ;
    end ;
  finally
    CloseFile(F);
  end;
end;

function TACBrCNIEE.BuscarECF(const AMarca, AModelo, AVersaoSB: String;
  const RemoveEspacos: Boolean): TACBrCNIEERegistro;
var
  I: Integer;
  Marca, Modelo, VersaoSB: String;
  MarcaAtual, ModeloAtual, VersaoAtual: String;
begin
  // abrir a tabela se estiver fechada
  if Cadastros.Count <= 0 then
  begin
    if not Self.AbrirTabela then
      raise EACBrCNIEE.Create('Não foi possível abrir a tabela de CNIEE.');
  end;

  Marca    := AnsiUpperCase(AMarca);
  Modelo   := AnsiUpperCase(AModelo);
  VersaoSB := AnsiUpperCase(ACBrUtil.OnlyNumber(AVersaoSB));

  if RemoveEspacos then
  begin
     Marca    := StringReplace(Marca,    ' ', '', [rfReplaceAll]);
     Modelo   := StringReplace(Modelo,   ' ', '', [rfReplaceAll]);
     VersaoSB := StringReplace(VersaoSB, ' ', '', [rfReplaceAll]);
  end;

  Result := nil;
  for I := 0 to Cadastros.Count - 1 do
  begin
    MarcaAtual  := AnsiUpperCase(Cadastros[I].DescrMarca);
    ModeloAtual := AnsiUpperCase(Cadastros[I].DescrModelo);
    VersaoAtual := AnsiUpperCase(ACBrUtil.OnlyNumber(Cadastros[I].Versao));

    if RemoveEspacos then
    begin
      MarcaAtual  := StringReplace(MarcaAtual,  ' ', '', [rfReplaceAll]);
      ModeloAtual := StringReplace(ModeloAtual, ' ', '', [rfReplaceAll]);
      VersaoAtual := StringReplace(VersaoAtual, ' ', '', [rfReplaceAll]);
    end;

    if (MarcaAtual = Marca) and (ModeloAtual = Modelo) and (VersaoAtual = VersaoSB) then
    begin
      Result := Cadastros[I];
      Exit;
    end;
  end;
end;

procedure TACBrCNIEE.Exportar(const AArquivo: String; ATipo: TACBrCNIEEExporta);
begin
  if Cadastros.Count <= 0 then
    Self.AbrirTabela;

  case ATipo of
    exTXT:  ExportarTXT(AArquivo);
    exCSV:  ExportarCSV(AArquivo);
    exDSV:  ExportarDSV(AArquivo);
    exXML:  ExportarXML(AArquivo);
    exHTML: ExportarHTML(AArquivo);
  end;
end;

procedure TACBrCNIEE.Exportar(const AArquivo, ADelimitador: String);
var
  I: Integer;
  Texto: String;
begin
  if Cadastros.Count <= 0 then
    Self.AbrirTabela;

  Texto := '';
  for I := 0 to Cadastros.Count - 1 do
  begin
    Texto := Texto +
      Cadastros[I].CodMarca + ADelimitador +
      Cadastros[I].CodModelo + ADelimitador +
      Cadastros[I].CodVersao + ADelimitador +
      Cadastros[I].TipoECF + ADelimitador +
      Cadastros[I].DescrMarca + ADelimitador +
      Cadastros[I].DescrModelo + ADelimitador +
      Cadastros[I].Versao + ADelimitador +
      IntToStr(Cadastros[I].QtLacresSL) + ADelimitador +
      IntToStr(Cadastros[I].QtLacresFab) + ADelimitador +
      Cadastros[I].TemMFD + ADelimitador +
      Cadastros[I].TemLacreMFD + ADelimitador +
      Cadastros[I].AtoAprovacao + ADelimitador +
      Cadastros[I].AtoRegistro + ADelimitador +
      Cadastros[I].FormatoNumFabricacao +
      sLineBreak;
  end;

  if Trim(Texto) <> '' then
    WriteToTXT(AnsiString(AArquivo), AnsiString(Texto), False, False);
end;

procedure TACBrCNIEE.ExportarTXT(const AArquivo: String);
var
  I: Integer;
  Texto: String;
begin
  Texto := '';
  for I := 0 to Cadastros.Count - 1 do
  begin
    Texto := Texto +
      PadRight(Cadastros[I].CodMarca, 2) +
      PadRight(Cadastros[I].CodModelo, 2) +
      PadRight(Cadastros[I].CodVersao, 2) +
      PadRight(Cadastros[I].TipoECF, 10) +
      PadRight(Cadastros[I].DescrMarca, 30) +
      PadRight(Cadastros[I].DescrModelo, 30) +
      PadRight(Cadastros[I].Versao, 20) +
      Format('%3.3d', [Cadastros[I].QtLacresSL]) +
      Format('%3.3d', [Cadastros[I].QtLacresFab]) +
      PadRight(Cadastros[I].TemMFD, 1) +
      PadRight(Cadastros[I].TemLacreMFD, 1) +
      PadRight(Cadastros[I].AtoAprovacao, 25) +
      PadRight(Cadastros[I].AtoRegistro, 25) +
      PadRight(Cadastros[I].FormatoNumFabricacao, 20) +
      sLineBreak;
  end;

  if Trim(Texto) <> '' then
    WriteToTXT(AnsiString(AArquivo), AnsiString(Texto), False, False);
end;

procedure TACBrCNIEE.ExportarCSV(const AArquivo: String);
var
  I: Integer;
  Texto: String;
begin
  Texto := '';
  for I := 0 to Cadastros.Count - 1 do
  begin
    Texto := Texto +
      Cadastros[I].CodMarca + ',' +
      Cadastros[I].CodModelo + ',' +
      Cadastros[I].CodVersao + ',' +
      Cadastros[I].TipoECF + ',' +
      Cadastros[I].DescrMarca + ',' +
      Cadastros[I].DescrModelo + ',' +
      Cadastros[I].Versao + ',' +
      IntToStr(Cadastros[I].QtLacresSL) + ',' +
      IntToStr(Cadastros[I].QtLacresFab) + ',' +
      Cadastros[I].TemMFD + ',' +
      Cadastros[I].TemLacreMFD + ',' +
      Cadastros[I].AtoAprovacao + ',' +
      Cadastros[I].AtoRegistro + ',' +
      Cadastros[I].FormatoNumFabricacao +
      sLineBreak;
  end;

  if Trim(Texto) <> '' then
    WriteToTXT(AnsiString(AArquivo), AnsiString(Texto), False, False);
end;

procedure TACBrCNIEE.ExportarDSV(const AArquivo: String);
var
  I: Integer;
  Texto: String;

  function AddAspasDuplas(const ATexto: String): String;
  begin
    Result := '"' + ATexto + '"';
  end;

begin
  Texto := '';
  for I := 0 to Cadastros.Count - 1 do
  begin
    Texto := Texto +
      AddAspasDuplas(Cadastros[I].CodMarca) + ',' +
      AddAspasDuplas(Cadastros[I].CodModelo) + ',' +
      AddAspasDuplas(Cadastros[I].CodVersao) + ',' +
      AddAspasDuplas(Cadastros[I].TipoECF) + ',' +
      AddAspasDuplas(Cadastros[I].DescrMarca) + ',' +
      AddAspasDuplas(Cadastros[I].DescrModelo) + ',' +
      AddAspasDuplas(Cadastros[I].Versao) + ',' +
      AddAspasDuplas(IntToStr(Cadastros[I].QtLacresSL)) + ',' +
      AddAspasDuplas(IntToStr(Cadastros[I].QtLacresFab)) + ',' +
      AddAspasDuplas(Cadastros[I].TemMFD) + ',' +
      AddAspasDuplas(Cadastros[I].TemLacreMFD) + ',' +
      AddAspasDuplas(Cadastros[I].AtoAprovacao) + ',' +
      AddAspasDuplas(Cadastros[I].AtoRegistro) + ',' +
      AddAspasDuplas(Cadastros[I].FormatoNumFabricacao) +
      sLineBreak;
  end;

  if Trim(Texto) <> '' then
    WriteToTXT(AnsiString(AArquivo), AnsiString(Texto), False, False);
end;

procedure TACBrCNIEE.ExportarXML(const AArquivo: String);
var
  I: Integer;
  Texto: String;
begin
  Texto := '<?xml version="1.0" encoding="ISO-8859-1"?><tabelacniee>';
  for I := 0 to Cadastros.Count - 1 do
  begin
    Texto := Texto +
      '<ecf>' +
      '<CodMarca>' + Cadastros[I].CodMarca + '</CodMarca>' +
      '<CodCodModelo>' + Cadastros[I].CodModelo + '</CodCodModelo>' +
      '<CodCodVersao>' + Cadastros[I].CodVersao + '</CodCodVersao>' +
      '<TipoECF>' + Cadastros[I].TipoECF + '</TipoECF>' +
      '<DescrMarca>' + Cadastros[I].DescrMarca + '</DescrMarca>' +
      '<DescrModelo>' + Cadastros[I].DescrModelo + '</DescrModelo>' +
      '<Versao>' + Cadastros[I].Versao + '</Versao>' +
      '<QtLacresSL>' + IntToStr(Cadastros[I].QtLacresSL) + '</QtLacresSL>' +
      '<QtLacresFab>' + IntToStr(Cadastros[I].QtLacresFab) + '</QtLacresFab>' +
      '<TemMFD>' + Cadastros[I].TemMFD + '</TemMFD>' +
      '<TemLacreMFD>' + Cadastros[I].TemLacreMFD + '</TemLacreMFD>' +
      '<AtoAprovacao>' + Cadastros[I].AtoAprovacao + '</AtoAprovacao>' +
      '<AtoRegistro>' + Cadastros[I].AtoRegistro + '</AtoRegistro>' +
      '<FormatoNumFabricacao>' + Cadastros[I].FormatoNumFabricacao + '</FormatoNumFabricacao>' +
      '</ecf>';
  end;
  Texto := Texto + '</tabelacniee>';

  if Trim(Texto) <> '' then
    WriteToTXT(AnsiString(AArquivo), AnsiString(Texto), False, True);
end;

procedure TACBrCNIEE.ExportarHTML(const AArquivo: String);
var
  I: Integer;
  Texto: String;
begin
  Texto :=
    '<html>' + slineBreak +
    '<head>' + slineBreak +
    '    <title>Tabela CNIEE</title>' + slineBreak +
    '    <style type="text/css">' + slineBreak +
    '        body{font-family: Arial;}' + slineBreak +
    '        th{color:white; font-size:8pt; background-color: black;}' + slineBreak +
		'        tr{font-size:8pt;}' + slineBreak +
    '        tr:nth-child(2n+1) {background-color: #DCDCDC;}' + slineBreak +
    '    </style>' + slineBreak +
    '</head>' + slineBreak +
    '<body>' + slineBreak +
    '    <table>' + slineBreak +
		'        <tr>' + slineBreak +
    '            <th>Marca</th>' + slineBreak +
    '            <th>Modelo</th>' + slineBreak +
    '            <th>Versao</th>' + slineBreak +
    '            <th>Tipo ECF</th>' + slineBreak +
    '            <th>Descrição Marca</th>' + slineBreak +
    '            <th>Descrição Modelo</th>' + slineBreak +
    '            <th>Versão</th>' + slineBreak +
    '            <th>Qt Lacres SL</th>' + slineBreak +
    '            <th>Qt Lacres Fab</th>' + slineBreak +
    '            <th>MFD</th>' + slineBreak +
    '            <th>Lacre MFD</th>' + slineBreak +
    '            <th>Ato Aprovação</th>' + slineBreak +
    '            <th>Ato Registro</th>' + slineBreak +
    '            <th>Formato Número Fabricação</th>' + slineBreak +
		'        </tr>' + slineBreak;

  for I := 0 to Cadastros.Count - 1 do
  begin
    Texto := Texto +
      '        <tr>' + slineBreak +
      '            <td>' + Cadastros[I].CodMarca + '</td>' + slineBreak +
      '            <td>' + Cadastros[I].CodModelo + '</td>' + slineBreak +
      '            <td>' + Cadastros[I].CodVersao + '</td>' + slineBreak +
      '            <td>' + Cadastros[I].TipoECF + '</td>' + slineBreak +
      '            <td>' + Cadastros[I].DescrMarca + '</td>' + slineBreak +
      '            <td>' + Cadastros[I].DescrModelo + '</td>' + slineBreak +
      '            <td>' + Cadastros[I].Versao + '</td>' + slineBreak +
      '            <td>' + IntToStr(Cadastros[I].QtLacresSL) + '</td>' + slineBreak +
      '            <td>' + IntToStr(Cadastros[I].QtLacresFab) + '</td>' + slineBreak +
      '            <td>' + Cadastros[I].TemMFD + '</td>' + slineBreak +
      '            <td>' + Cadastros[I].TemLacreMFD + '</td>' + slineBreak +
      '            <td>' + Cadastros[I].AtoAprovacao + '</td>' + slineBreak +
      '            <td>' + Cadastros[I].AtoRegistro + '</td>' + slineBreak +
      '            <td>' + Cadastros[I].FormatoNumFabricacao + '</td>' + slineBreak +
      '        </tr>' + slineBreak;
  end;

  Texto := Texto +
    '    </table>' + slineBreak +
    '</body>' + slineBreak +
    '</html>' + slineBreak;

  if Trim(Texto) <> '' then
    WriteToTXT(AnsiString(AArquivo), AnsiString(Texto), False, True);
end;

end.
