unit UPrincipal;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, ExtDlgs, ExtCtrls, ACBrDevice, ACBrETQ;

type

  { TFPrincipal }

  TFPrincipal = class(TForm)
    ACBrETQ: TACBrETQ;
    bCarregarImg: TButton;
    bEtqBloco: TButton;
    bEtqCarreiras: TButton;
    bEtqSimples: TButton;
    bImprimirImagem: TButton;
    cbBackFeed: TComboBox;
    cbOrigem: TComboBox;
    cbDPI: TComboBox;
    cbModelo: TComboBox;
    cbPorta: TComboBox;
    ckMemoria: TCheckBox;
    eAvanco: TEdit;
    eCopias: TEdit;
    edNomeImg: TEdit;
    eTemperatura: TEdit;
    eMargemEsquerda: TEdit;
    eVelocidade: TEdit;
    gbConfiguracao: TGroupBox;
    gbImagem: TGroupBox;
    gbImpressao: TGroupBox;
    Image1: TImage;
    lbAvanco: TLabel;
    lbBackFeed: TLabel;
    lbBackFeed1: TLabel;
    lbCopias: TLabel;
    lbDPI: TLabel;
    lbModelo: TLabel;
    lbNomeImg: TLabel;
    lbPorta: TLabel;
    lbTemperatura: TLabel;
    lbMargem: TLabel;
    lbTemperatura2: TLabel;
    OpenPictureDialog1: TOpenPictureDialog;
    rbArquivo: TRadioButton;
    rbStream: TRadioButton;
    procedure bEtqBlocoClick(Sender: TObject);
    procedure bEtqSimplesClick(Sender: TObject);
    procedure bEtqCarreirasClick(Sender: TObject);
    procedure bImprimirImagemClick(Sender : TObject);
    procedure bCarregarImgClick(Sender : TObject);
    procedure cbModeloChange(Sender : TObject) ;
    procedure eCopiasKeyPress(Sender : TObject ; var Key : char) ;
    procedure FormCreate(Sender: TObject);
  private
     procedure AtivarACBrETQ ;
     procedure ImprimirEtiquetaComCopiasEAvanco;
    { private declarations }
  public
    { public declarations }
  end; 

var
  FPrincipal: TFPrincipal;

implementation
uses
  typinfo, Printers, synautil,
  ACBrUtil, ACBrImage
  {$IfDef MSWINDOWS}
  ,ACBrWinUSBDevice
  {$EndIf};

{$R *.lfm}

procedure TFPrincipal.FormCreate(Sender: TObject);
var
  I : TACBrETQModelo ;
  J: TACBrETQDPI;
  K: TACBrETQUnidade;
  L: TACBrETQBackFeed;
  M: Integer;
  N: TACBrETQOrigem;
begin
  cbModelo.Items.Clear ;
  For I := Low(TACBrETQModelo) to High(TACBrETQModelo) do
     cbModelo.Items.Add( GetEnumName(TypeInfo(TACBrETQModelo), integer(I) ) ) ;

  cbDPI.Items.Clear ;
  For J := Low(TACBrETQDPI) to High(TACBrETQDPI) do
     cbDPI.Items.Add( GetEnumName(TypeInfo(TACBrETQDPI), integer(J) ) ) ;

  cbBackFeed.Items.Clear ;
  For L := Low(TACBrETQBackFeed) to High(TACBrETQBackFeed) do
     cbBackFeed.Items.Add( GetEnumName(TypeInfo(TACBrETQBackFeed), integer(L) ) ) ;

  cbOrigem.Items.Clear ;
  For N := Low(TACBrETQOrigem) to High(TACBrETQOrigem) do
     cbOrigem.Items.Add( GetEnumName(TypeInfo(TACBrETQOrigem), integer(N) ) ) ;

  cbPorta.Items.Clear;
  ACBrETQ.Device.AcharPortasSeriais( cbPorta.Items );

  {$IfDef MSWINDOWS}
   ACBrETQ.Device.WinUSB.FindUSBPrinters();
   for M := 0 to ACBrETQ.Device.WinUSB.DeviceList.Count-1 do
     cbPorta.Items.Add('USB:'+ACBrETQ.Device.WinUSB.DeviceList.Items[M].DeviceName);
  {$EndIf}

  cbPorta.Items.Add('LPT1') ;
  cbPorta.Items.Add('\\localhost\L42') ;
  cbPorta.Items.Add('c:\temp\teste.txt') ;
  cbPorta.Items.Add('TCP:192.168.0.31:9100') ;

  For M := 0 to Printer.Printers.Count-1 do
    cbPorta.Items.Add('RAW:'+Printer.Printers[M]);

  {$IfNDef MSWINDOWS}
  cbPorta.Items.Add('/dev/ttyS0') ;
  cbPorta.Items.Add('/dev/ttyS1') ;
  cbPorta.Items.Add('/dev/ttyUSB0') ;
  cbPorta.Items.Add('/dev/ttyUSB1') ;
  cbPorta.Items.Add('/tmp/ecf.txt') ;
  {$EndIf}

  cbDPI.ItemIndex := 0;
  cbModelo.ItemIndex := 1;
  cbPorta.ItemIndex := 16;
end;

procedure TFPrincipal.bEtqSimplesClick(Sender: TObject);
begin
  AtivarACBrETQ ;

  with ACBrETQ do
  begin
     if Modelo in [etqPpla, etqPplb] then
      begin
        ImprimirTexto(orNormal, 2, 2, 2, 3, 3, 'BISCOITO MARILAN RECH 335G', 0, True);
        ImprimirTexto(orNormal, 2, 2, 1, 8, 3, 'CHOC BRANCO');
        ImprimirBarras(orNormal, barEAN13, 2, 2, 13, 5, '7896003701685', 10, becSIM);
        ImprimirCaixa(13,32,56,14,1,1);
        ImprimirTexto(orNormal, 3, 3, 2, 18, 35, 'R$');
        ImprimirTexto(orNormal, 3, 4, 4, 15, 50, '20,59');
      end
      else  //if Modelo = etqZPLII then
      begin
        ImprimirCaixa(3,3,90,5,5,0);
        ImprimirTexto(orNormal, 'T', 10, 10, 3, 3, 'BISCOITO MARILAN RECH 335G', 0, True);
        ImprimirTexto(orNormal, 'S', 10, 10, 8, 3, 'CHOC BRANCO');
        ImprimirBarras(orNormal, barEAN13, 2, 2, 13, 5, '7896003701685', 10, becSIM);
        ImprimirCaixa(13,32,56,17,1,1);
        ImprimirTexto(orNormal, 'G', 40, 80, 18, 35, 'R$');
        ImprimirTexto(orNormal, 'G', 55, 100, 15, 50, '20,59');
      end;

     ImprimirEtiquetaComCopiasEAvanco;
     Desativar;
  end;
end;

procedure TFPrincipal.ImprimirEtiquetaComCopiasEAvanco;
begin
  ACBrETQ.Imprimir(StrToIntDef(eCopias.Text, 1), StrToIntDef(eAvanco.Text, 0));
end;

procedure TFPrincipal.bEtqBlocoClick(Sender: TObject);

  procedure FinalizarEtiquetaComCopiasEAvanco;
  begin
    ACBrETQ.FinalizarEtiqueta(StrToIntDef(eCopias.Text, 1), StrToIntDef(eAvanco.Text, 0));
  end;

begin
  AtivarACBrETQ;

  with ACBrETQ do
  begin
     if Modelo in [etqPpla, etqPplb] then
     begin
       IniciarEtiqueta;
       ImprimirTexto(orNormal, 2, 2, 2, 3, 3, 'BISCOITO MARILAN RECH 335G', 0, True);
       ImprimirTexto(orNormal, 2, 2, 1, 8, 3, 'CHOC BRANCO');
       ImprimirBarras(orNormal, barEAN13, 2, 2, 13, 5, '7896003701685', 10, becSIM);
       ImprimirTexto(orNormal, 3, 3, 2, 18, 35, 'R$');
       ImprimirTexto(orNormal, 3, 4, 4, 15, 50, '20,59');
       FinalizarEtiquetaComCopiasEAvanco;

       IniciarEtiqueta;
       ImprimirTexto(orNormal, 2, 2, 2, 3, 3, 'SABAO EM PO FLASH 1KG', 0, True);
       ImprimirTexto(orNormal, 2, 2, 1, 8, 3, 'ADVANCED - UNIDADE');
       ImprimirBarras(orNormal, barEAN13, 2, 2, 13, 5, '7898903097042', 10, becSIM);
       ImprimirTexto(orNormal, 3, 3, 2, 18, 35, 'R$');
       ImprimirTexto(orNormal, 3, 4, 4, 15, 50, '3,18');
       FinalizarEtiquetaComCopiasEAvanco;

       IniciarEtiqueta;
       ImprimirTexto(orNormal, 2, 2, 2, 3, 3, 'AMACIANTE AMACIEX 5 LTS', 0, True);
       ImprimirTexto(orNormal, 2, 2, 1, 8, 3, 'MACIO MATRIX FIX');
       ImprimirBarras(orNormal, barEAN13, 2, 2, 13, 5, '7898237690230', 10, becSIM);
       ImprimirTexto(orNormal, 3, 3, 2, 18, 35, 'R$');
       ImprimirTexto(orNormal, 3, 4, 4, 15, 50, '8,60');
       FinalizarEtiquetaComCopiasEAvanco;
     end
     else //if Modelo = etqZPLII then
     begin
       IniciarEtiqueta;
       ImprimirCaixa(3,3,90,5,5,0);
       ImprimirTexto(orNormal, 'T', 10, 10, 3, 3, 'BISCOITO MARILAN RECH 335G', 0, True);
       ImprimirTexto(orNormal, 'S', 10, 10, 8, 3, 'CHOC BRANCO');
       ImprimirBarras(orNormal, barEAN13, 2, 2, 13, 5, '7896003701685', 10, becSIM);
       ImprimirTexto(orNormal, 'G', 40, 80, 18, 35, 'R$');
       ImprimirTexto(orNormal, 'G', 55, 100, 15, 50, '20,59');
       FinalizarEtiquetaComCopiasEAvanco;

       IniciarEtiqueta;
       ImprimirCaixa(3,3,90,5,5,0);
       ImprimirTexto(orNormal, 'T', 10, 10, 3, 3, 'SABAO EM PO FLASH 1KG', 0, True);
       ImprimirTexto(orNormal, 'S', 10, 10, 8, 3, 'ADVANCED - UNIDADE');
       ImprimirBarras(orNormal, barEAN13, 2, 2, 13, 5, '7898903097042', 10, becSIM);
       ImprimirTexto(orNormal, 'G', 40, 80, 18, 35, 'R$');
       ImprimirTexto(orNormal, 'G', 55, 100, 15, 50, '3,18');
       FinalizarEtiquetaComCopiasEAvanco;

       IniciarEtiqueta;
       ImprimirCaixa(3,3,90,5,5,0);
       ImprimirTexto(orNormal, 'T', 10, 10, 3, 3, 'AMACIANTE AMACIEX 5 LTS', 0, True);
       ImprimirTexto(orNormal, 'S', 10, 10, 8, 3, 'MACIO MATRIX FIX');
       ImprimirBarras(orNormal, barEAN13, 2, 2, 13, 5, '7898237690230', 10, becSIM);
       ImprimirTexto(orNormal, 'G', 40, 80, 18, 35, 'R$');
       ImprimirTexto(orNormal, 'G', 55, 100, 15, 50, '8,60');
       FinalizarEtiquetaComCopiasEAvanco;
     end;

     Imprimir(1, StrToIntDef(eAvanco.Text, 0));
     Desativar;
  end;
end;

procedure TFPrincipal.bEtqCarreirasClick(Sender: TObject);
begin
  AtivarACBrETQ;

  with ACBrETQ do
  begin
     if Modelo in [etqPpla, etqPplb] then
      begin
        ImprimirTexto(orNormal, 2, 1, 2, 2, 3, 'BISCOITO REC 33G');
        ImprimirTexto(orNormal, 2, 1, 1, 6, 3, 'CHOC BRANCO');
        ImprimirBarras(orNormal, barEAN13, 2, 2, 8, 3, '7896003701685', 10);

        ImprimirTexto(orNormal, 2, 1, 2, 2, 32, 'BISCOITO RECH 33G');
        ImprimirTexto(orNormal, 2, 1, 1, 6, 32, 'CHOC BRANCO');
        ImprimirBarras(orNormal, barEAN13, 2, 2, 8, 32, '7896003701685', 10);

        ImprimirTexto(orNormal, 2, 1, 2, 2, 61, 'BISCOITO RECH 33G');
        ImprimirTexto(orNormal, 2, 1, 1, 6, 61, 'CHOC BRANCO');
        ImprimirBarras(orNormal, barEAN13, 2, 2, 8, 61, '7896003701685', 10);
      end
     else // if Modelo = etqZPLII then
      begin
         ImprimirTexto(orNormal, '0', 20, 30, 2, 3, 'BISCOITO REC 33G');
         ImprimirTexto(orNormal, '0', 20, 20, 6, 3, 'CHOC BRANCO');
         ImprimirBarras(orNormal, barEAN13, 2, 2, 8, 3, '7896003701685', 10);

         ImprimirTexto(orNormal, '0', 20, 30, 2, 32, 'BISCOITO RECH 33G');
         ImprimirTexto(orNormal, '0', 20, 20, 6, 32, 'CHOC BRANCO');
         ImprimirBarras(orNormal, barEAN13, 2, 2, 8, 32, '7896003701685', 10);

         ImprimirTexto(orNormal, '0', 20, 30, 2, 61, 'BISCOITO RECH 33G');
         ImprimirTexto(orNormal, '0', 20, 20, 6, 61, 'CHOC BRANCO');
         ImprimirBarras(orNormal, barEAN13, 2, 2, 8, 61, '7896003701685', 10);
      end;

      ImprimirEtiquetaComCopiasEAvanco;
      Desativar;
  end;
end;

procedure TFPrincipal.bImprimirImagemClick(Sender : TObject);
begin
  AtivarACBrETQ;

  with ACBrETQ do
  begin
     ImprimirImagem(1,10,10,edNomeImg.Text);
     ImprimirEtiquetaComCopiasEAvanco;
     Desativar;
  end ;
end;

procedure TFPrincipal.bCarregarImgClick(Sender : TObject);
var
  MS : TMemoryStream;
  OK: Boolean;
begin
  if (edNomeImg.Text = '') then
  begin
    ShowMessage('Defina um nome para a Imagem');
    Exit;
  end;

  AtivarACBrETQ;

  OK := False;
  OpenPictureDialog1.InitialDir := ExtractFileDir(Application.ExeName);

  case ACBrETQ.Modelo of
    etqPplb: OpenPictureDialog1.Filter := 'PCX|*.pcx';
  else
    OpenPictureDialog1.Filter := 'PCX|*.pcx|BMP MonoCromático|*.bmp';
  end;

  if rbStream.Checked then
   begin
     if (Image1.Picture.Bitmap.Empty) then
     begin
       if OpenPictureDialog1.Execute then
       begin
         try
           Image1.Picture.LoadFromFile(OpenPictureDialog1.FileName);
         except
           Image1.Picture.Clear;
         end ;
       end;
     end;

     MS := TMemoryStream.Create;
     try
       Image1.Picture.SaveToStream(MS);
       MS.Position := 0;
       ACBrETQ.CarregarImagem( MS, edNomeImg.Text, True, ExtractFileExt(OpenPictureDialog1.FileName) );
       OK := True;
     finally
       MS.Free ;
     end ;
   end
  else
   begin
     if OpenPictureDialog1.Execute then
     begin
       ACBrETQ.CarregarImagem( OpenPictureDialog1.FileName, edNomeImg.Text );
       OK := True;
       try
         Image1.Picture.LoadFromFile(OpenPictureDialog1.FileName);
       except
         Image1.Picture.Clear;
       end ;
     end ;
   end ;

  if OK then
    MessageDlg('Imagem '+edNomeImg.Text+', carregada na memória da Impressora', mtInformation,[mbOK],0);

  ACBrETQ.Desativar;
end;

procedure TFPrincipal.cbModeloChange(Sender : TObject) ;
begin
   eAvanco.Enabled := (cbModelo.ItemIndex = 1);
   ACBrETQ.Desativar;
end;

procedure TFPrincipal.eCopiasKeyPress(Sender : TObject ; var Key : char) ;
begin
   if not (Key in ['0'..'9',#8,#13]) then
      Key := #0 ;
end;

procedure TFPrincipal.AtivarACBrETQ;
begin
  with ACBrETQ do
  begin
     Desativar;

     DPI           := TACBrETQDPI(cbDPI.ItemIndex);
     Modelo        := TACBrETQModelo(cbModelo.ItemIndex);
     Porta         := cbPorta.Text;
     LimparMemoria := ckMemoria.Checked;
     Temperatura   := StrToIntDef(eTemperatura.Text,10);
     Velocidade    := StrToIntDef(eVelocidade.Text,-1);
     BackFeed      := TACBrETQBackFeed(cbBackFeed.ItemIndex);
     Unidade       := etqMilimetros; //etqDecimoDeMilimetros;
     MargemEsquerda:= StrToIntDef(eMargemEsquerda.Text, 0);
     Origem        := TACBrETQOrigem(cbOrigem.ItemIndex);

     Ativar;
  end;
end;

end.

