{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{																			   }
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

unit Frm_Download;

//{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Buttons, ComCtrls,
  ACBrBase, ACBrDownload, ACBrDownloadClass,
  httpsend, blcksock;

type
  TForm1 = class(TForm)
    bDownload: TBitBtn;
    bStop: TBitBtn;
    bPause: TBitBtn;
    Button1: TButton;
    cbxBufferSize: TComboBox;
    edURL: TComboBox;
    edFile: TEdit;
    Label1: TLabel;
    lConnectionInfo: TLabel;
    lFile: TLabel;
    ProgressBar1: TProgressBar;
    Label2: TLabel;
    edtProt: TComboBox;
    GroupBox1: TGroupBox;
    Label3: TLabel;
    edtHost: TEdit;
    Label4: TLabel;
    edtPort: TEdit;
    Label5: TLabel;
    edtUser: TEdit;
    Label6: TLabel;
    edtPass: TEdit;
    GroupBox2: TGroupBox;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    edtProxyHost: TEdit;
    edtProxyPort: TEdit;
    edtProxyUser: TEdit;
    edtProxyPass: TEdit;
    fACBrDownload: TACBrDownload;
    CheckBox1: TCheckBox;
    Label11: TLabel;
    edArq: TEdit;
    procedure bStopClick(Sender: TObject);
    procedure bDownloadClick(Sender: TObject);
    procedure bPauseClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure cbxBufferSizeChange(Sender: TObject);
    procedure edtProtChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure HookMonitor(Sender: TObject; const BytesToDownload,
      BytesDownloaded: Integer; const AverageSpeed: Double; const Hour, Min,
      Sec: Word);
    procedure HookStatus(Sender: TObject; Reason: THookSocketReason;
      const BytesToDownload, BytesDownloaded: Integer);
    procedure fACBrDownloadAfterDownload(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

{ TForm1 }

procedure TForm1.bDownloadClick(Sender: TObject);
begin
   if edtProt.Text = 'HTTP' then
   begin
      if Pos('WWW', UpperCase(edURL.Text)) = 0 then
      begin
         ShowMessage('O Link não corresponde ao protocolo selecionado!');
         Exit;
      end;
   end
   else
   if edtProt.Text = 'FTP' then
   begin
      if Pos('FTP', UpperCase(edURL.Text)) = 0 then
      begin
         ShowMessage('O Link não corresponde ao protocolo selecionado!');
         Exit;
      end;
   end;

   fACBrDownload.Proxy.ProxyHost := edtProxyHost.Text;
   fACBrDownload.Proxy.ProxyPort := edtProxyPort.Text;
   fACBrDownload.Proxy.ProxyUser := edtProxyUser.Text;
   fACBrDownload.Proxy.ProxyPass := edtProxyPass.Text;

   fACBrDownload.FTP.FtpHost     := edtHost.Text;
   fACBrDownload.FTP.FtpPort     := edtPort.Text;
   fACBrDownload.FTP.FtpUser     := edtUser.Text;
   fACBrDownload.FTP.FtpPass     := edtPass.Text;

   fACBrDownload.DownloadDest    := edFile.Text;
   fACBrDownload.DownloadNomeArq := edArq.Text;
   fACBrDownload.DownloadUrl     := edURL.Text;

   fACBrDownload.StartDownload;
end;

procedure TForm1.bPauseClick(Sender: TObject);
begin
   fACBrDownload.DownloadStatus := stPause;
end;

procedure TForm1.Button1Click(Sender: TObject);
Var
  SL : TStringList;
  I: Integer;
begin
  SL := TStringList.Create;

  For I := 1 to 10240 do
  begin
    SL.Add( 'LINHA '+Format('%.5d',[I])+'...+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....0' );
  end;
  SL.SaveToFile('BigFile.txt');
end;

procedure TForm1.cbxBufferSizeChange(Sender: TObject);
begin
   fACBrDownload.SizeRecvBuffer := StrToInt(cbxBufferSize.Text);

   // Isto não parece funcionar ... Como posso mudar fHTTPSend.Sock.SizeRecvBuffer para HTTP?
end;

procedure TForm1.edtProtChange(Sender: TObject);
begin
   if edtProt.Text = 'HTTP' then
      fACBrDownload.Protocolo := protHTTP
   else
   if edtProt.Text = 'FTP' then
      fACBrDownload.Protocolo := protFTP;

   GroupBox1.Enabled := edtProt.Text = 'FTP';
end;

procedure TForm1.fACBrDownloadAfterDownload(Sender: TObject);
begin
   if fACBrDownload.DownloadStatus = stDownload then
   begin
      ShowMessage('Download concluído com sucesso!');

      if CheckBox1.Checked then
         Self.Close;
   end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
   edtProt.OnChange(Sender)
end;

procedure TForm1.bStopClick(Sender: TObject);
begin
   fACBrDownload.DownloadStatus := stStop;
end;

procedure TForm1.HookMonitor(Sender: TObject; const BytesToDownload: Integer;
     const BytesDownloaded: Integer; const AverageSpeed: Double;
     const Hour: Word; const Min: Word; const Sec: Word);
var
  sConnectionInfo: string;
begin
  ProgressBar1.Position := BytesDownloaded;

  sConnectionInfo := sConnectionInfo + '  -  ' +
                     Format('%.2d:%.2d:%.2d', [Sec div 3600, (Sec div 60) mod 60, Sec mod 60]);

  sConnectionInfo := FormatFloat('0.00 KB/s'  , AverageSpeed) + sConnectionInfo;
  sConnectionInfo := FormatFloat('###,###,##0', BytesDownloaded / 1024) + ' / ' +
                     FormatFloat('###,###,##0', BytesToDownload / 1024) +' KB  -  ' + sConnectionInfo;

  lConnectionInfo.Caption := sConnectionInfo;
end;

procedure TForm1.HookStatus(Sender: TObject; Reason: THookSocketReason;
  const BytesToDownload, BytesDownloaded: Integer);
begin
   case Reason of
     HR_Connect :
     begin
       ProgressBar1.Position := 0;
       bDownload.Enabled := False;
       bPause.Enabled    := True;
       bStop.Enabled     := True;
     end;
     HR_ReadCount :
     begin
       ProgressBar1.Max        := BytesToDownload;
       ProgressBar1.Position   := BytesDownloaded;
//       lConnectionInfo.Caption := 'Baixando...';
     end;
     HR_SocketClose :
     begin
       case fACBrDownload.DownloadStatus of
         stStop :
         begin
           ProgressBar1.Position  := 0;
           lConnectionInfo.Caption := 'Download Encerrado...';
         end;

         stPause :
           lConnectionInfo.Caption := 'Download Pausado...';

         stDownload :
//           lConnectionInfo.Caption := 'Download Finalizado.';

       end;
       bDownload.Enabled := True;
       bPause.Enabled    := False;
       bStop.Enabled     := False;
     end;
   end;
end;


end.

