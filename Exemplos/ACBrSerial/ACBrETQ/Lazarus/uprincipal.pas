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
  private
     procedure AtivarACBrETQ ;
    { private declarations }
  public
    { public declarations }
  end; 

var
  FPrincipal: TFPrincipal;

implementation

{$R *.lfm}

procedure TFPrincipal.bEtqSimplesClick(Sender: TObject);
begin
  AtivarACBrETQ ;

  with ACBrETQ do
  begin
     if Modelo = etqPpla then
      begin
        ImprimirTexto(orNormal, 2, 2, 2, 190, 5, 'BISCOITO MARILAN RECH 335G', 0, True);
        ImprimirTexto(orNormal, 2, 2, 1, 158, 5, 'CHOC BRANCO');
        ImprimirBarras(orNormal, 'F', '2', '2', 32, 0, '7896003701685', 90, becSIM);
        ImprimirTexto(orNormal, 3, 3, 2, 15, 300, 'R$');
        ImprimirTexto(orNormal, 3, 4, 4, 15, 450, '20.59');
      end
     else if Modelo = etqZPLII then
      begin
        ImprimirTexto(orNormal, 'Q', 1, 1, 15, 55, 'BISCOITO MARILAN RECH 335G', 0, True);
        ImprimirTexto(orNormal, 'Q', 1, 1, 60, 55, 'CHOC BRANCO');
        ImprimirBarras(orNormal, barEAN13, 2, 2, 95, 55, '7896003701685', 90, becSIM);
        ImprimirTexto(orNormal, 'R', 1, 1, 100, 270, 'R$');
        ImprimirTexto(orNormal, 'R', 1, 1, 135, 270, '20.59');
      end
     else
      begin
        ImprimirTexto(orNormal, 2, 2, 2, 15, 55, 'BISCOITO MARILAN RECH 335G', 0, True);
        ImprimirTexto(orNormal, 3, 2, 1, 60, 55, 'CHOC BRANCO');
        ImprimirBarras(orNormal, 'E30', '2', '2', 95, 55, '7896003701685', 90, becSIM);
        ImprimirTexto(orNormal, 3, 3, 2, 110, 355, 'R$');
        ImprimirTexto(orNormal, 3, 4, 5, 85, 515, '20.59');
      end ;

     Imprimir(StrToInt(eCopias.Text), StrToInt(eAvanco.Text));
     Desativar;
  end;
end;

procedure TFPrincipal.bEtqBlocoClick(Sender: TObject);
begin
  AtivarACBrETQ;

  with ACBrETQ do
  begin
     if Modelo = etqPpla then
     begin
       IniciarEtiqueta;
       ImprimirTexto(orNormal, 2, 2, 2, 190, 5, 'BISCOITO MARILAN RECH 335G');
       ImprimirTexto(orNormal, 2, 2, 1, 158, 5, 'CHOC BRANCO');
       ImprimirBarras(orNormal, 'F', '2', '2', 32, 0, '7896003701685', 90, becSIM);
       ImprimirTexto(orNormal, 3, 3, 2, 15, 300, 'R$');
       ImprimirTexto(orNormal, 3, 4, 4, 15, 450, '20.59');
       FinalizarEtiqueta(StrToInt(eCopias.Text), StrToInt(eAvanco.Text));

       IniciarEtiqueta;
       ImprimirTexto(orNormal, 2, 2, 2, 190, 5, 'SABAO EM PO FLASH 1KG');
       ImprimirTexto(orNormal, 2, 2, 1, 158, 5, 'ADVANCED - UNIDADE');
       ImprimirBarras(orNormal, 'F', '2', '2', 32, 0, '7898903097042', 90, becSIM);
       ImprimirTexto(orNormal, 3, 3, 2, 15, 300, 'R$');
       ImprimirTexto(orNormal, 3, 4, 4, 15, 450, '3.18');
       FinalizarEtiqueta(StrToInt(eCopias.Text), StrToInt(eAvanco.Text));

       IniciarEtiqueta;
       ImprimirTexto(orNormal, 2, 2, 2, 190, 5, 'AMACIANTE AMACIEX 5 LTS');
       ImprimirTexto(orNormal, 2, 2, 1, 158, 5, 'MACIO MATRIX FIX');
       ImprimirBarras(orNormal, 'F', '2', '2', 32, 0, '7898237690230', 90, becSIM);
       ImprimirTexto(orNormal, 3, 3, 2, 15, 300, 'R$');
       ImprimirTexto(orNormal, 3, 4, 4, 15, 450, '8.60');
       FinalizarEtiqueta(StrToInt(eCopias.Text), StrToInt(eAvanco.Text));
     end
     else
     begin
       IniciarEtiqueta;
       ImprimirTexto(orNormal, 2, 2, 2, 15, 55, 'BISCOITO MARILAN RECH 335G');
       ImprimirTexto(orNormal, 3, 2, 1, 60, 55, 'CHOC BRANCO');
       ImprimirBarras(orNormal, 'E30', '2', '2', 95, 55, '7896003701685', 90, becSIM);
       ImprimirTexto(orNormal, 3, 3, 2, 110, 355, 'R$');
       ImprimirTexto(orNormal, 3, 4, 5, 85, 515, '20.59');
       FinalizarEtiqueta(StrToInt(eCopias.Text), StrToInt(eAvanco.Text));

       IniciarEtiqueta;
       ImprimirTexto(orNormal, 2, 2, 2, 15, 55, 'SABAO EM PO FLASH 1KG');
       ImprimirTexto(orNormal, 3, 2, 1, 60, 55, 'ADVANCED - UNIDADE');
       ImprimirBarras(orNormal, 'E30', '2', '2', 95, 55, '7898903097042', 90, becSIM);
       ImprimirTexto(orNormal, 3, 3, 2, 110, 355, 'R$');
       ImprimirTexto(orNormal, 3, 4, 5, 85, 515, '3.18');
       FinalizarEtiqueta(StrToInt(eCopias.Text), StrToInt(eAvanco.Text));

       IniciarEtiqueta;
       ImprimirTexto(orNormal, 2, 2, 2, 15, 55, 'AMACIANTE AMACIEX 5 LTS');
       ImprimirTexto(orNormal, 3, 2, 1, 60, 55, 'MACIO MATRIX FIX');
       ImprimirBarras(orNormal, 'E30', '2', '2', 95, 55, '7898237690230', 90, becSIM);
       ImprimirTexto(orNormal, 3, 3, 2, 110, 355, 'R$');
       ImprimirTexto(orNormal, 3, 4, 5, 85, 515, '8.60');
       FinalizarEtiqueta(StrToInt(eCopias.Text), StrToInt(eAvanco.Text));
     end;

     Imprimir(1, StrToInt(eAvanco.Text));
     Desativar;
  end;
end;

procedure TFPrincipal.bEtqCarreirasClick(Sender: TObject);
begin
  AtivarACBrETQ;

  with ACBrETQ do
  begin
     if Modelo = etqPpla then
      begin
        ImprimirTexto(orNormal, 2, 1, 2, 180, 15, 'BISCOITO REC 335G');
        ImprimirTexto(orNormal, 2, 1, 1, 140, 15, 'CHOC BRANCO');
        ImprimirBarras(orNormal, 'F', '2', '2', 20, 10, '7896003701685', 70);

        ImprimirTexto(orNormal, 2, 1, 2, 180, 315, 'BISCOITO RECH 335G');
        ImprimirTexto(orNormal, 2, 1, 1, 140, 315, 'CHOC BRANCO');
        ImprimirBarras(orNormal, 'F', '2', '2', 20, 315, '7896003701685', 70);

        ImprimirTexto(orNormal, 2, 1, 2, 180, 620, 'BISCOITO RECH 335G');
        ImprimirTexto(orNormal, 2, 1, 1, 140, 620, 'CHOC BRANCO');
        ImprimirBarras(orNormal, 'F', '2', '2', 20, 620, '7896003701685', 70);
      end
     else if Modelo = etqZPLII then
      begin
        ImprimirTexto(orNormal, 'Q', 1, 1, 15, 55, 'BISCOITO REC 335G');
        ImprimirTexto(orNormal, 'Q', 1, 1, 60, 55, 'CHOC BRANCO');
        ImprimirBarras(orNormal, barEAN13, 2, 2, 95, 55, '7896003701685', 90, becSIM);

        ImprimirTexto(orNormal, 'Q', 1, 1, 15, 365, 'BISCOITO REC 335G');
        ImprimirTexto(orNormal, 'Q', 1, 1, 60, 365, 'CHOC BRANCO');
        ImprimirBarras(orNormal, barEAN13, 2, 2, 95, 365, '7896003701685', 90, becSIM);

        ImprimirTexto(orNormal, 'Q', 1, 1, 15, 670, 'BISCOITO REC 335G');
        ImprimirTexto(orNormal, 'Q', 1, 1, 60, 670, 'CHOC BRANCO');
        ImprimirBarras(orNormal, barEAN13, 2, 2, 95, 670, '7896003701685', 90, becSIM);
      end
     else
      begin
        ImprimirTexto(orNormal, 2, 1, 3, 15, 55, 'BISCOITO REC 335G');
        ImprimirTexto(orNormal, 2, 1, 1, 80, 55, 'CHOC BRANCO');
        ImprimirBarras(orNormal, 'E30', '2', '2', 120, 55, '7896003701685', 080, becSIM);

        ImprimirTexto(orNormal, 2, 1, 3, 15, 365, 'BISCOITO RECH 335G');
        ImprimirTexto(orNormal, 2, 1, 1, 80, 365, 'CHOC BRANCO');
        ImprimirBarras(orNormal, 'E30', '2', '2', 120, 365, '7896003701685', 080, becSIM);

        ImprimirTexto(orNormal, 2, 1, 3, 15, 670, 'BISCOITO RECH 335G');
        ImprimirTexto(orNormal, 2, 1, 1, 80, 670, 'CHOC BRANCO');
        ImprimirBarras(orNormal, 'E30', '2', '2', 120, 670, '7896003701685', 080, becSIM);
      end ;

     Imprimir(StrToInt(eCopias.Text), StrToInt(eAvanco.Text));
     Desativar;
  end;
end;

procedure TFPrincipal.bImprimirImagemClick(Sender : TObject);
begin
  AtivarACBrETQ;

  with ACBrETQ do
  begin
     ImprimirImagem(1,10,10,edNomeImg.Text);
     Imprimir(StrToInt(eCopias.Text), StrToInt(eAvanco.Text));
     Desativar;
  end ;
end;

procedure TFPrincipal.bCarregarImgClick(Sender : TObject);
var
   MS : TMemoryStream ;
begin
  AtivarACBrETQ;

  OpenPictureDialog1.InitialDir := ExtractFileDir(Application.ExeName);

  if rbStream.Checked then
   begin
     OpenPictureDialog1.Filter := 'BMP MonoCromático|*.bmp' ;
     if OpenPictureDialog1.Execute then
     begin
        Image1.Picture.LoadFromFile(OpenPictureDialog1.FileName) ;
        MS := TMemoryStream.Create;
        try
           Image1.Picture.SaveToStream( MS );
           ACBrETQ.CarregarImagem( MS, edNomeImg.Text, True,
                                   ExtractFileExt(OpenPictureDialog1.FileName) );
        finally
           MS.Free ;
        end ;
     end ;
   end
  else
   begin
      OpenPictureDialog1.Filter := 'PCX|*.pcx|BMP MonoCromático|*.bmp|IMG|*.img' ;
      if OpenPictureDialog1.Execute then
      begin
         try
            Image1.Picture.LoadFromFile(OpenPictureDialog1.FileName);
         except
            Image1.Picture.Clear;
         end ;
         ACBrETQ.CarregarImagem( OpenPictureDialog1.FileName, edNomeImg.Text );
      end ;
   end ;

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
     DPI           := TACBrETQDPI(cbDPI.ItemIndex);
     Modelo        := TACBrETQModelo(cbModelo.ItemIndex);
     Porta         := cbPorta.Text;
     LimparMemoria := ckMemoria.Checked;
     Temperatura   := StrToInt(eTemperatura.Text);
     Velocidade    := StrToInt(eVelocidade.Text);
     BackFeed      := TACBrETQBackFeed(cbBackFeed.ItemIndex);

     Ativar;
  end;
end;

end.

