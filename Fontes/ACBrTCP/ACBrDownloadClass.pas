{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2004 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Isaque Pinheiro                                 }
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
|* 27/10/2011: Primeira Versao
|*    Isaque Pinheiro e Daniel Simoes de Almeida
|*    Criaçao do componente ACBrDownload, que implementa de fazer download de
|*    arquivos via http e ftp, com recurso de pausa e continuação do download.
******************************************************************************}

{$I ACBr.inc}

unit ACBrDownloadClass;

interface

uses
  Classes, SysUtils, Forms,
  blcksock;

type
  TMemory = pointer;

  TDownloadStatus = (stNone, stStop, stDownload, stPause, stResume, stRedirect);

  TACBrHookMonitor = procedure(Sender: TObject; const BytesToDownload: Integer;
     const BytesDownloaded: Integer; const AverageSpeed: Double;
     const Hour: Word; const Min: Word; const Sec: Word) of object;

  TACBrHookStatus = procedure(Sender: TObject; Reason: THookSocketReason;
     const BytesToDownload: Integer; const BytesDownloaded: Integer) of object;
  TACBrDownloadStatus = procedure(Sender: TObject;
     const DownloadStatus: TDownloadStatus) of object;

  TACBrBeforeDownload = procedure(Sender: TObject) of object;
  TACBrAfterDownload = procedure(Sender: TObject) of object;

  TACBrFTP = class(TPersistent)
  private
    fFtpHost: string;
    fFtpPort: string;
    fFtpUser: string;
    fFtpPass: string;
    fFtpTimeout: Integer;
  public
    constructor Create;

  published
    property FtpHost: string read fFtpHost write fFtpHost;
    property FtpPort: string read fFtpPort write fFtpPort;
    property FtpUser: string read fFtpUser write fFtpUser;
    property FtpPass: string read fFtpPass write fFtpPass;
    property FtpTimeout: Integer read fFtpTimeout write fFtpTimeout;
  end;

  TACBrProxy = class(TPersistent)
  private
    fProxyHost: string;
    fProxyPort: string;
    fProxyUser: string;
    fProxyPass: string;
    fProxyTimeout: Integer;
  public
    constructor Create;

  published
    property ProxyHost: string read fProxyHost write fProxyHost;
    property ProxyPort: string read fProxyPort write fProxyPort;
    property ProxyUser: string read fProxyUser write fProxyUser;
    property ProxyPass: string read fProxyPass write fProxyPass;
    property ProxyTimeout: Integer read fProxyTimeout write fProxyTimeout;
  end;

  TACBrDownloadClass = class(TPersistent)
  private
    fBytesToDownload: Integer;
    fStartTime: TDateTime;
    fAverageSpeed: Double;
    fSizeRecvBuffer: Integer;
    fDownloadDest: string;

    fOnHookStatus: TACBrHookStatus;
    fOnHookMonitor: TACBrHookMonitor;
    fOnDownloadStatus: TACBrDownloadStatus;

    procedure SyncDocToFile;
    procedure SetSizeRecvBuffer(const Value: Integer);
    procedure SetDownloadStatus(const Value: TDownloadStatus);
  protected
    fOwner: TComponent;
    fProxy: TACBrProxy;
    fSock: TTCPBlockSocket;
    fDocument: TMemoryStream;

    fFileStream: TFileStream;
    fDownloadStatus: TDownloadStatus;
    fBytesDownloaded: Integer;
    fFilePart: String;
    fDownloadSize: Integer;
    fResultCode: Integer;
    fDownloadUrl: string;
    fDownloadNomeArq :string;
    fBytesResumed: Integer;

    fOnBeforeDownload: TACBrBeforeDownload;
    fOnAfterDownload: TACBrAfterDownload;

    procedure DoHookStatus(Sender: TObject; Reason: THookSocketReason;
      const Value: String); virtual;
    procedure DoHookMonitor(Sender: TObject; Writing: Boolean;
      const Buffer: TMemory; Len: Integer); virtual;
    procedure OpenCreateFile;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;

    procedure StartDownload; virtual;
  published
    property DownloadDest: string read fDownloadDest write fDownloadDest;
    property DownloadUrl: string read fDownloadUrl write fDownloadUrl;
    property DownloadNomeArq : string read fDownloadNomeArq write fDownloadNomeArq;
    property DownloadStatus: TDownloadStatus read fDownloadStatus write SetDownloadStatus;
    property SizeRecvBuffer: Integer read fSizeRecvBuffer write SetSizeRecvBuffer;
    property Proxy: TACBrProxy read fProxy write fProxy;
    // Eventos
    property OnBeforeDownload: TACBrBeforeDownload read fOnBeforeDownload write fOnBeforeDownload;
    property OnAfterDownload: TACBrAfterDownload read fOnAfterDownload write fOnAfterDownload;
    property OnHookStatus: TACBrHookStatus read fOnHookStatus write fOnHookStatus;
    property OnHookMonitor: TACBrHookMonitor read fOnHookMonitor write fOnHookMonitor;
    property OnDownloadStatus: TACBrDownloadStatus read fOnDownloadStatus write fOnDownloadStatus;
  end;

implementation

uses ACBrDownload, ACBrUtil;

{ TACBrDownloadClass }

constructor TACBrDownloadClass.Create(AOwner: TComponent);
begin

   if not (AOwner is TACBrDownload) then
      raise Exception.Create('Essa Classe deve ser instanciada por TACBrDownload');
   inherited Create;
   fOwner := AOwner;

   fFileStream := nil;

   fBytesDownloaded := 0;
   fBytesToDownload := 0;
   fBytesResumed    := 0;
   fStartTime       := 0;
   fAverageSpeed    := 0;
   fDownloadSize    := 0;
   fFilePart        := '';

   // Propriedades
   fProxy           := TACBrDownload(fOwner).Proxy;
   fDownloadDest    := TACBrDownload(fOwner).DownloadDest;
   fDownloadUrl     := TACBrDownload(fOwner).DownloadUrl;
   fDownloadNomeArq := TACBrDownload(fOwner).DownloadNomeArq;
   fDownloadStatus  := TACBrDownload(fOwner).DownloadStatus;
   if Assigned(fSock) then
      SizeRecvBuffer := TACBrDownload(fOwner).SizeRecvBuffer;
   // Eventos
   fOnBeforeDownload := TACBrDownload(fOwner).OnBeforeDownload;
   fOnAfterDownload  := TACBrDownload(fOwner).OnAfterDownload;
   fOnHookStatus     := TACBrDownload(fOwner).OnHookStatus;
   fOnHookMonitor    := TACBrDownload(fOwner).OnHookMonitor;
   fOnDownloadStatus := TACBrDownload(fOwner).OnDownloadStatus;
end;

destructor TACBrDownloadClass.Destroy;
begin
   if Assigned(fSock) then
   begin
      fSock.OnStatus  := nil;
      fSock.OnMonitor := nil;
      fSock := nil;
   end;
   if Assigned(fDocument) then
      fDocument := nil;

   if Assigned(fProxy) then
      fProxy := nil;

   if Assigned(fOwner) then
      fOwner := nil;

   if Assigned(fFileStream) then
      fFileStream.Free;

  inherited Destroy;
end;

procedure TACBrDownloadClass.DoHookMonitor(Sender: TObject; Writing: Boolean;
  const Buffer: TMemory; Len: Integer);
var
  dTotalTime: TDateTime;
  wHour, wMin, wSec, wMSec: Word;
  nDLTime: Double;
begin
  if Writing then
     Exit;

  fBytesDownloaded := fBytesDownloaded + Len;

  if Assigned(fFileStream) then
  begin
     if fDocument.Size > (fFileStream.Size - fBytesResumed) then
        SyncDocToFile;
  end;

  dTotalTime := Now - fStartTime;
  DecodeTime(dTotalTime, wHour, wMin, wSec, wMSec);
  wSec := wSec + wMin * 60 + wHour * 3600;
  nDLTime := wSec + wMSec / 1000;
  if nDLTime > 0 then
     fAverageSpeed := (fBytesDownloaded / 1024) / nDLTime;

  if fAverageSpeed > 0 then
  begin
     wSec := Trunc(((fBytesToDownload - fBytesDownloaded) / 1024) / fAverageSpeed);
  end;
  if fDownloadStatus in [stPause, stStop] then
     fSock.CloseSocket;

  // Evento
  if Assigned(fOnHookMonitor) then
     fOnHookMonitor(Sender,
                    fBytesToDownload,
                    fBytesDownloaded,
                    fAverageSpeed,
                    wHour,
                    wMin,
                    wSec);
  Application.ProcessMessages;
end;

procedure TACBrDownloadClass.DoHookStatus(Sender: TObject; Reason: THookSocketReason;
  const Value: String);
var
  sFileName: String;
begin
  case Reason of
     HR_Connect:
     begin
        // Evento
        if Assigned(fOnHookStatus) then
           fOnHookStatus(Sender, Reason, fBytesToDownload, fBytesDownloaded);
     end;
     HR_ReadCount:
     begin
       if not (fDownloadStatus in [stNone, stResume]) then
          Exit;

       if fDownloadSize = 0  then
          Exit;

       if (fDownloadStatus = stNone) or (fStartTime = 0) then
       begin
          fStartTime    := Now;
          fAverageSpeed := 0;
       end;
       fDownloadStatus  := stDownload;
       fBytesDownloaded := fBytesResumed;
       fBytesToDownload := fBytesResumed + fDownloadSize;
       // Evento
       if Assigned(fOnHookStatus) then
          fOnHookStatus(Sender, Reason, fBytesToDownload, fBytesDownloaded);
     end;
     HR_SocketClose:
     begin
       // Codigos
       // 150 Pause e Stop FTP
       // 200 Stop HTTP ou fim de download HTTP
       // 206 Pause HTTP ou fim de download HTTP
       // 226 Fim de download FTP
       if (fResultCode = 206) or (fResultCode = 200) or
          (fResultCode = 150) or (fResultCode = 125) then
       begin
          if Assigned(fFileStream) then
          begin
             SyncDocToFile;
             FreeAndNil(fFileStream);
          end;
       end;

       case fDownloadStatus of
         stStop:
         begin
            if not DeleteFile(fFilePart) then
              raise Exception.Create(Format('Não foi possível excluir o arquivo: %s', [fFilePart]));
         end;
         stPause:
         begin

         end;
         stDownload, stNone:
         begin
            // Só renomeia o arquivo se finalizou o download, senão deixa
            // o arquivo com o nome.ext.part para tentar continuar de onde parou
            if (fResultCode = 200) or (fResultCode = 206) or (fResultCode = 226) then
            begin
              sFileName := Copy(fFilePart, 1, Length(fFilePart) -5);
              if FileExists(sFileName) and FileExists(fFilePart) then
                if not DeleteFile(sFileName) then
                  raise Exception.Create(Format('Não foi possível excluir o arquivo: %s', [sFileName]));

              if FileExists(fFilePart) then              
                if not RenameFile(fFilePart, sFileName) then
                  raise Exception.Create(Format('Não foi possível renomear o arquivo: %s para %s', [fFilePart, sFileName]));
            end;
         end
       end;
       // Evento
       if Assigned(fOnHookStatus) then
          fOnHookStatus(Sender, Reason, fBytesToDownload, fBytesDownloaded);
     end;
  end;
end;

procedure TACBrDownloadClass.SetSizeRecvBuffer(const Value: Integer);
begin
  fSizeRecvBuffer := Value;
  if Assigned(fSock) then
     fSock.SizeRecvBuffer := Value;
end;

procedure TACBrDownloadClass.SetDownloadStatus(const Value: TDownloadStatus);
begin
  fDownloadStatus := Value;
  // Evento
  if Assigned(fOnDownloadStatus) then
     fOnDownloadStatus(Self, Value);
end;

procedure TACBrDownloadClass.StartDownload;
begin

end;

procedure TACBrDownloadClass.OpenCreateFile;
begin
  fFilePart := PathWithDelim(fDownloadDest) + fFilePart;
  if FileExists(fFilePart) then
  begin
    fDownloadStatus := stResume;
    fFileStream := TFileStream.Create(fFilePart, fmOpenReadWrite or fmShareExclusive);
    fFileStream.Seek(0, soFromEnd);
    fBytesResumed := fFileStream.Size;
  end
  else
  begin
    fDownloadStatus := stNone;
    fFileStream := TFileStream.Create(fFilePart, fmCreate or fmShareExclusive);
    fFileStream.Seek(0, soFromBeginning);
    fBytesResumed := 0;
  end;
end;

procedure TACBrDownloadClass.SyncDocToFile;
var
  iDiff: Integer;
  iDocSize : Integer;
begin
   try
     iDocSize := fDocument.Size;
     if iDocSize = 0 then
        Exit;

     iDiff := iDocSize - (fFileStream.Size-fBytesResumed);
     if iDiff > 0 then
     begin
        fDocument.Seek(-iDiff, soFromEnd);
        fFileStream.CopyFrom(fDocument, iDiff );
     end;
   finally
     fDocument.Seek(0, soFromEnd);
   end;
end;

{ TACBrProxy }

constructor TACBrProxy.Create;
begin
   inherited Create;
   fProxyHost := '';
   fProxyPort := '';
   fProxyUser := '';
   fProxyPass := '';
   // Mantém como padrão a configuração feita no Create de THTTPSend
   fProxyTimeout := 90000;
end;

{ TACBrFTP }

constructor TACBrFTP.Create;
begin
   inherited Create;
   fFtpHost := '';
   fFtpPort := '';
   fFtpUser := '';
   fFtpPass := '';
   // Mantém como padrão a configuração feita no Create de TFTPSend
   fFtpTimeout := 300000;
end;

end.
