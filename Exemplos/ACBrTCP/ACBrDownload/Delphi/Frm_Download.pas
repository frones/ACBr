{************************************************* **************************}
{Copyright (c) 2010 Daniel Simões de Almeida e Isaque Pinheiro               }
{                                                                            }
{ Este software é fornecido "como está". Este software vem sem garantia      }
{ Ou garantia, expressa ou implícita. Use este software em seu próprio risco.}
{ O autor não será responsável por quaisquer danos aos equipamentos, dados ou}
{ Informações que possam resultar enquanto usando este software.             }
{                                                                            }
{Ao utilizar este software, você concorda com as condições acima referidas.  }
{************************************************* **************************}

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

