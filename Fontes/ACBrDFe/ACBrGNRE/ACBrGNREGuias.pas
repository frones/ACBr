{******************************************************************************}
{ Projeto: Componente ACBrGNRE                                                 }
{  Biblioteca multiplataforma de componentes Delphi/Lazarus para emissão da    }
{  Guia Nacional de Recolhimento de Tributos Estaduais                         }
{  http://www.gnre.pe.gov.br/                                                  }
{                                                                              }
{ Direitos Autorais Reservados (c) 2013 Claudemir Vitor Pereira                }
{                                       Daniel Simoes de Almeida               }
{                                       André Ferreira de Moraes               }
{                                       Juliomar Marchetti                     }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do Projeto ACBr     }
{ Componentes localizado em http://www.sourceforge.net/projects/acbr           }
{                                                                              }
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
|* 09/12/2013 - Claudemir Vitor Pereira
|*  - Doação do componente para o Projeto ACBr
******************************************************************************}
{$I ACBr.inc}

unit ACBrGNREGuias;

interface

uses
  Classes, Sysutils, Dialogs, Forms,
  ACBrGNREUtil,
  ACBrGNREConfiguracoes,
//  {$IFDEF FPC}
//     ACBrNFSeDMLaz,
//  {$ELSE}
//     ACBrNFSeDANFSeClass,
//  {$ENDIF}
  smtpsend, ssl_openssl, mimemess, mimepart, // units para enviar email
  pgnreGNRE, pgnreGNRER, pgnreGNREW, pgnreConversao,
  pcnConversao, pcnAuxiliar, pcnLeitor;

type

  Guia = class(TCollectionItem)
  private
    FGNRE: TGNRE;
    FConfirmada : Boolean;
    FMsg : AnsiString ;
    FAlertas: AnsiString;
    FNomeArq: String;
    FXML: AnsiString;
    function GetGNREXML: AnsiString;
  public
    constructor Create(Collection2: TCollection); override;
    destructor Destroy; override;
    function SaveToFile(CaminhoArquivo: string = ''): boolean;
    function SaveToStream(Stream: TStringStream): boolean;
    property GNRE: TGNRE  read FGNRE write FGNRE;
    property Confirmada: Boolean  read FConfirmada write FConfirmada;
    property Msg: AnsiString  read FMsg write FMsg;
    property Alertas: AnsiString read FAlertas write FAlertas;
    property NomeArq: String read FNomeArq write FNomeArq;
    property XML: AnsiString  read GetGNREXML write FXML;
  end;

 TGuias = class(TOwnedCollection)
  private
    FConfiguracoes : TConfiguracoes;
    FACBrGNRE : TComponent;
    function GetItem(Index: Integer): Guia;
    procedure SetItem(Index: Integer; const Value: Guia);
  public
    constructor Create(AOwner: TPersistent; ItemClass: TCollectionItemClass);
    procedure GerarGNRE;
    function Add: Guia;
    function Insert(Index: Integer): Guia;
    property Items[Index: Integer]: Guia read GetItem  write SetItem;
    property Configuracoes: TConfiguracoes read FConfiguracoes  write FConfiguracoes;

    function GetNamePath: string; override ;
    function LoadFromFile(CaminhoArquivo: string): boolean;
    function LoadFromStream(Stream: TStringStream): boolean;
    function SaveToFile(PathArquivo: string = ''): boolean;
    property ACBrGNRE : TComponent read FACBrGNRE ;
  end;

  TSendMailThread = class(TThread)
  private
    FException : Exception;
    procedure DoHandleException;
  public
    OcorreramErros: Boolean;
    Terminado: Boolean;
    smtp : TSMTPSend;
    sFrom : String;
    sTo : String;
    sCC : TStrings;
    slmsg_Lines : TStrings;
    constructor Create(AOwner: Guia);
    destructor Destroy; override;
  protected
    procedure Execute; override;
    procedure HandleException;
  end;

implementation

uses
 ACBrGNRE2, ACBrUtil, ACBrDFeUtil, pcnGerador;

{ Guia }

constructor Guia.Create(Collection2: TCollection);
begin
 inherited Create(Collection2);
 FGNRE     := TGNRE.Create;
 FNomeArq  := '';
end;

destructor Guia.Destroy;
begin
  FGNRE.Free;
  inherited Destroy;
end;

function Guia.GetGNREXML: AnsiString;
var LocGNREW : TGNREW;
begin
  LocGNREW := TGNREW.Create(Self.GNRE);
  try
    LocGNREW.GerarXml;
    Result := LocGNREW.Gerador.ArquivoFormatoXML;
  finally
    LocGNREW.Free;
  end;
end;

function Guia.SaveToFile(CaminhoArquivo: string): boolean;
var LocGNREW : TGNREW;
begin
  try
    Result   := True;
    LocGNREW := TGNREW.Create(GNRE);
    try
      LocGNREW.GerarXml;

      if EstaVazio(CaminhoArquivo) then
      begin
        if not EstaVazio(GNRE.c42_identificadorGuia) then
          CaminhoArquivo := GNREUtil.PathWithDelim(TACBrGNRE( TGuias( Collection ).ACBrGNRE ).Configuracoes.Geral.PathSalvar) + GNRE.c42_identificadorGuia + '-gnre.xml'
        else
          CaminhoArquivo := GNREUtil.PathWithDelim(TACBrGNRE( TGuias( Collection ).ACBrGNRE ).Configuracoes.Geral.PathSalvar) + FormatDateTime('yyyymmddhhnnss',Now) + '-gnre.xml'
      end;

      if EstaVazio(CaminhoArquivo) or not DirectoryExists(ExtractFilePath(CaminhoArquivo))
        then raise Exception.Create('Caminho Inválido: ' + CaminhoArquivo);

      LocGNREW.Gerador.SalvarArquivo(CaminhoArquivo);
      NomeArq := CaminhoArquivo;
    finally
      LocGNREW.Free;
    end;
  except
    raise;
    Result := False;
  end;
end;

function Guia.SaveToStream(Stream: TStringStream): boolean;
var
  LocGNREW : TGNREW;
begin
  try
    Result   := True;
    LocGNREW := TGNREW.Create(GNRE);
    try
      LocGNREW.GerarXml;
      Stream.WriteString(LocGNREW.Gerador.ArquivoFormatoXML);
    finally
      LocGNREW.Free;
    end;
  except
    raise;
    Result := False;
  end;
end;

{ TGuias }

function TGuias.Add: Guia;
begin
  Result := Guia(inherited Add);
end;

constructor TGuias.Create(AOwner: TPersistent;
  ItemClass: TCollectionItemClass);
begin
 if not (AOwner is TACBrGNRE )
  then raise Exception.Create( 'AOwner deve ser do tipo TGNRE.') ;

 inherited;
 FACBrGNRE     := TACBrGNRE( AOwner ) ;
end;

procedure TGuias.GerarGNRE;
var i       : Integer;
    LocGNREW: TGNREW;
begin
  for i := 0 to Self.Count-1 do
  begin
    LocGNREW := TGNREW.Create(Self.Items[i].GNRE);
    try
      LocGNREW.GerarXml;
      Self.Items[i].XML := LocGNREW.Gerador.ArquivoFormatoXML;
      Self.Items[i].Alertas := LocGNREW.Gerador.ListaDeAlertas.Text;
    finally
      LocGNREW.Free;
    end;
  end;
end;

function TGuias.GetItem(Index: Integer): Guia;
begin
  Result := Guia(inherited Items[Index]);
end;

function TGuias.GetNamePath: string;
begin
  Result := 'Guia';
end;

function TGuias.Insert(Index: Integer): Guia;
begin
  Result := Guia(inherited Insert(Index));
end;

function TGuias.LoadFromFile(CaminhoArquivo: string): boolean;
var LocGNRER : TGNRER;
  ArquivoXML: TStringList;
  XML : AnsiString;
begin
  try
    ArquivoXML := TStringList.Create;
    ArquivoXML.LoadFromFile(CaminhoArquivo);
    Result := True;

    ArquivoXML.Text := StringReplace(StringReplace( ArquivoXML.Text, '&lt;', '<', [rfReplaceAll]), '&gt;', '>', [rfReplaceAll]);
    ArquivoXML.Text := GNREUtil.RetirarPrefixos(ArquivoXML.Text);

    while pos('</guias>',ArquivoXML.Text) > 0 do
    begin
      XML := copy(ArquivoXML.Text,1,pos('</guias>',ArquivoXML.Text) + 5);
      ArquivoXML.Text := Trim(copy(ArquivoXML.Text,pos('</guias>',ArquivoXML.Text) + 6,length(ArquivoXML.Text)));

      LocGNRER := TGNRER.Create(Self.Add.GNRE);
      try
        LocGNRER.Leitor.Arquivo := XML;
        LocGNRER.LerXml;
        Items[Self.Count-1].XML := LocGNRER.Leitor.Arquivo;
        Items[Self.Count-1].NomeArq := CaminhoArquivo;
        GerarGNRE;
      finally
        LocGNRER.Free;
      end;
    end;

    ArquivoXML.Free;
  except
    raise;
    Result := False;
  end;
end;

function TGuias.LoadFromStream(Stream: TStringStream): boolean;
var LocGNRER : TGNRER;
  ArquivoXML: TStringList;
begin
  try
    Result     := True;
    LocGNRER   := TGNRER.Create(Self.Add.GNRE);
    ArquivoXML := TStringList.Create;
    try
      LocGNRER.Leitor.CarregarArquivo(Stream);
      ArquivoXML.Text := LocGNRER.Leitor.Arquivo;
      ArquivoXML.Text := StringReplace(StringReplace( ArquivoXML.Text, '&lt;', '<', [rfReplaceAll]), '&gt;', '>', [rfReplaceAll]);
      ArquivoXML.Text := GNREUtil.RetirarPrefixos(ArquivoXML.Text);

      LocGNRER.Leitor.CarregarArquivo(TStringStream.Create(ArquivoXML.Text));
      LocGNRER.LerXml;
      Items[Self.Count-1].XML := LocGNRER.Leitor.Arquivo;
      GerarGNRE;
    finally
      LocGNRER.Free
    end;
    ArquivoXML.Free;
  except
    Result := False;
  end;
end;

function TGuias.SaveToFile(PathArquivo: string): boolean;
var i : integer;
  CaminhoArquivo : String;
begin
  Result := True;
  try
    for i := 0 to TACBrGNRE( FACBrGNRE ).Guias.Count-1 do
    begin
      if EstaVazio(PathArquivo) then
        PathArquivo := TACBrGNRE( FACBrGNRE ).Configuracoes.Geral.PathSalvar
      else
        PathArquivo := ExtractFilePath(PathArquivo);

      CaminhoArquivo := GNREUtil.PathWithDelim(PathArquivo) + FormatDateTime('yyyymmddhhnnss',Now)+'-LoteGNRE.xml';
      TACBrGNRE( FACBrGNRE ).Guias.Items[i].SaveToFile(CaminhoArquivo);
    end;
  except
    Result := False;
  end;
end;

procedure TGuias.SetItem(Index: Integer; const Value: Guia);
begin
  Items[Index].Assign(Value);
end;

{ TSendMailThread }

constructor TSendMailThread.Create(AOwner: Guia);
begin
 smtp        := TSMTPSend.Create;
 slmsg_Lines := TStringList.Create;
 sCC         := TStringList.Create;
 sFrom       := '';
 sTo         := '';

 FreeOnTerminate := True;

  inherited Create(True);
end;

destructor TSendMailThread.Destroy;
begin
 slmsg_Lines.Free;
 sCC.Free;
 smtp.Free;

  inherited;
end;

procedure TSendMailThread.DoHandleException;
begin
 if FException is Exception
  then Application.ShowException(FException)
  else SysUtils.ShowException(FException, nil);
end;

procedure TSendMailThread.Execute;
var
 I: integer;
begin
  inherited;

 try
  Terminado := False;
  try
   if not smtp.Login()
    then raise Exception.Create('SMTP ERROR: Login:' + smtp.EnhCodeString+sLineBreak+smtp.FullResult.Text);

   if not smtp.MailFrom( sFrom, Length(sFrom))
    then raise Exception.Create('SMTP ERROR: MailFrom:' + smtp.EnhCodeString+sLineBreak+smtp.FullResult.Text);

   if not smtp.MailTo(sTo)
    then raise Exception.Create('SMTP ERROR: MailTo:' + smtp.EnhCodeString+sLineBreak+smtp.FullResult.Text);

   if (sCC <> nil)
    then begin
     for I := 0 to sCC.Count - 1 do
      begin
       if not smtp.MailTo(sCC.Strings[i])
        then raise Exception.Create('SMTP ERROR: MailTo:' + smtp.EnhCodeString+sLineBreak+smtp.FullResult.Text);
      end;
    end;

   if not smtp.MailData(slmsg_Lines)
    then raise Exception.Create('SMTP ERROR: MailData:' + smtp.EnhCodeString+sLineBreak+smtp.FullResult.Text);

   if not smtp.Logout()
    then raise Exception.Create('SMTP ERROR: Logout:' + smtp.EnhCodeString+sLineBreak+smtp.FullResult.Text);
  finally
   try
    smtp.Sock.CloseSocket;
   except
   end ;
   Terminado := True;
  end;
 except
  Terminado := True;
  HandleException;
 end;
end;

procedure TSendMailThread.HandleException;
begin
 FException := Exception(ExceptObject);
 try
  // Não mostra mensagens de EAbort
  if not (FException is EAbort)
   then Synchronize(DoHandleException);
 finally
  FException := nil;
 end;
end;

end.
