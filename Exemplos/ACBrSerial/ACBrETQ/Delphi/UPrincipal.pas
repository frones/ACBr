unit UPrincipal;

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, ExtDlgs, ExtCtrls, ACBrDevice, ACBrETQ, ACBrBase;

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
    cbDPI: TComboBox;
    cbModelo: TComboBox;
    cbPorta: TComboBox;
    ckMemoria: TCheckBox;
    eAvanco: TEdit;
    eCopias: TEdit;
    edNomeImg: TEdit;
    eTemperatura: TEdit;
    eVelocidade: TEdit;
    gbConfiguracao: TGroupBox;
    gbImagem: TGroupBox;
    gbImpressao: TGroupBox;
    Image1: TImage;
    lbAvanco: TLabel;
    lbBackFeed: TLabel;
    lbCopias: TLabel;
    lbDPI: TLabel;
    lbModelo: TLabel;
    lbNomeImg: TLabel;
    lbPorta: TLabel;
    lbTemperatura: TLabel;
    lbTemperatura1: TLabel;
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
  Printers, typinfo;

{$R *.dfm}

procedure TFPrincipal.FormCreate(Sender: TObject);
var
  I : TACBrETQModelo ;
  J: TACBrETQDPI;
//  K: TACBrETQUnidade;
  L: TACBrETQBackFeed;
  M: Integer;
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

  ACBrETQ.Device.AcharPortasSeriais( cbPorta.Items );
  cbPorta.Items.Add('LPT1') ;
  cbPorta.Items.Add('\\localhost\L42') ;
  cbPorta.Items.Add('c:\temp\teste.txt') ;
  cbPorta.Items.Add('TCP:192.168.0.31:9100') ;

  For M := 0 to Printer.Printers.Count-1 do
    cbPorta.Items.Add('RAW:'+Printer.Printers[M]);

  cbDPI.ItemIndex := 0;
  cbModelo.ItemIndex := 1;
  cbPorta.ItemIndex := 0;
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
        ImprimirTexto(orNormal, 3, 3, 2, 18, 32, 'R$');
        ImprimirTexto(orNormal, 3, 4, 4, 15, 50, '20,59');
      end
     else  //if Modelo = etqZPLII then
      begin
        ImprimirTexto(orNormal, '0', 60, 60, 3, 3, 'BISCOITO MARILAN RECH 335G', 0, True);
        ImprimirTexto(orNormal, '0', 60, 60, 8, 3, 'CHOC BRANCO');
        ImprimirBarras(orNormal, barEAN13, 2, 2, 13, 5, '7896003701685', 10, becSIM);
        ImprimirTexto(orNormal, 'A', 100, 100, 18, 32, 'R$');
        ImprimirTexto(orNormal, 'A', 120, 120, 15, 50, '20,59');
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
       ImprimirTexto(orNormal, 3, 3, 2, 18, 32, 'R$');
       ImprimirTexto(orNormal, 3, 4, 4, 15, 50, '20,59');
       FinalizarEtiquetaComCopiasEAvanco;

       IniciarEtiqueta;
       ImprimirTexto(orNormal, 2, 2, 2, 3, 3, 'SABAO EM PO FLASH 1KG');
       ImprimirTexto(orNormal, 2, 2, 1, 8, 3, 'ADVANCED - UNIDADE');
       ImprimirBarras(orNormal, barEAN13, 2, 2, 13, 5, '7898903097042', 10, becSIM);
       ImprimirTexto(orNormal, 3, 3, 2, 18, 32, 'R$');
       ImprimirTexto(orNormal, 3, 4, 4, 15, 50, '3,18');
       FinalizarEtiquetaComCopiasEAvanco;

       IniciarEtiqueta;
       ImprimirTexto(orNormal, 2, 2, 2, 3, 3, 'AMACIANTE AMACIEX 5 LTS');
       ImprimirTexto(orNormal, 2, 2, 1, 8, 3, 'MACIO MATRIX FIX');
       ImprimirBarras(orNormal, barEAN13, 2, 2, 13, 5, '7898237690230', 10, becSIM);
       ImprimirTexto(orNormal, 3, 3, 2, 18, 32, 'R$');
       ImprimirTexto(orNormal, 3, 4, 4, 15, 50, '8,60');
       FinalizarEtiquetaComCopiasEAvanco;
     end
     else //if Modelo = etqZPLII then
     begin
       IniciarEtiqueta;
       ImprimirTexto(orNormal, '0', 60, 60, 3, 3, 'BISCOITO MARILAN RECH 335G', 0, True);
       ImprimirTexto(orNormal, '0', 60, 60, 8, 3, 'CHOC BRANCO');
       ImprimirBarras(orNormal, barEAN13, 2, 2, 13, 5, '7896003701685', 10, becSIM);
       ImprimirTexto(orNormal, 'A', 100, 100, 18, 32, 'R$');
       ImprimirTexto(orNormal, 'A', 120, 120, 15, 50, '20,59');
       FinalizarEtiquetaComCopiasEAvanco;

       IniciarEtiqueta;
       ImprimirTexto(orNormal, '0', 60, 60, 3, 3, 'SABAO EM PO FLASH 1KG', 0, True);
       ImprimirTexto(orNormal, '0', 60, 60, 8, 3, 'ADVANCED - UNIDADE');
       ImprimirBarras(orNormal, barEAN13, 2, 2, 13, 5, '7898903097042', 10, becSIM);
       ImprimirTexto(orNormal, 'A', 100, 100, 18, 32, 'R$');
       ImprimirTexto(orNormal, 'A', 120, 120, 15, 50, '3,18');
       FinalizarEtiquetaComCopiasEAvanco;

       IniciarEtiqueta;
       ImprimirTexto(orNormal, '0', 60, 60, 3, 3, 'AMACIANTE AMACIEX 5 LTS', 0, True);
       ImprimirTexto(orNormal, '0', 60, 60, 8, 3, 'MACIO MATRIX FIX');
       ImprimirBarras(orNormal, barEAN13, 2, 2, 13, 5, '7898237690230', 10, becSIM);
       ImprimirTexto(orNormal, 'A', 100, 100, 18, 32, 'R$');
       ImprimirTexto(orNormal, 'A', 120, 120, 15, 50, '8,60');
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

        ImprimirTexto(orNormal, 2, 1, 2, 2, 28, 'BISCOITO RECH 33G');
        ImprimirTexto(orNormal, 2, 1, 1, 6, 28, 'CHOC BRANCO');
        ImprimirBarras(orNormal, barEAN13, 2, 2, 8, 28, '7896003701685', 10);

        ImprimirTexto(orNormal, 2, 1, 2, 2, 53, 'BISCOITO RECH 33G');
        ImprimirTexto(orNormal, 2, 1, 1, 6, 53, 'CHOC BRANCO');
        ImprimirBarras(orNormal, barEAN13, 2, 2, 8, 53, '7896003701685', 10);
      end
     else // if Modelo = etqZPLII then
      begin
         ImprimirTexto(orNormal, '0', 30, 40, 2, 3, 'BISCOITO REC 33G');
         ImprimirTexto(orNormal, '0', 20, 20, 6, 3, 'CHOC BRANCO');
         ImprimirBarras(orNormal, barEAN13, 2, 2, 8, 3, '7896003701685', 10);

         ImprimirTexto(orNormal, '0', 30, 40, 2, 28, 'BISCOITO RECH 33G');
         ImprimirTexto(orNormal, '0', 20, 20, 6, 28, 'CHOC BRANCO');
         ImprimirBarras(orNormal, barEAN13, 2, 2, 8, 28, '7896003701685', 10);

         ImprimirTexto(orNormal, '0', 30, 40, 2, 53, 'BISCOITO RECH 33G');
         ImprimirTexto(orNormal, '0', 20, 20, 6, 53, 'CHOC BRANCO');
         ImprimirBarras(orNormal, barEAN13, 2, 2, 8, 53, '7896003701685', 10);
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
   MS : TMemoryStream ;
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
     if (Image1.Picture.Width = 0) then
     begin
       if OpenPictureDialog1.Execute then
       begin
         try
           Image1.Picture.LoadFromFile(OpenPictureDialog1.FileName);
         except
           Image1.Picture := nil;
         end ;
       end;
     end;

     MS := TMemoryStream.Create;
     try
       Image1.Picture.Bitmap.SaveToStream(MS);
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
         Image1.Picture := nil;
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
     Temperatura   := StrToInt(eTemperatura.Text);
     Velocidade    := StrToInt(eVelocidade.Text);
     BackFeed      := TACBrETQBackFeed(cbBackFeed.ItemIndex);
     Unidade       := etqMilimetros;

     Ativar;
  end;
end;

end.

