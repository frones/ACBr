unit UForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, ACBrBase, ACBrDFe,
  ACBrNFSe, ACBrNFSeX;

type
  TForm1 = class(TForm)
    btn1: TButton;
    OpenDialog1: TOpenDialog;
    ACBrNFSeX1: TACBrNFSeX;
    procedure btn1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  ACBrNFSeXDANFSeFPDFClass;

{$R *.dfm}

procedure TForm1.btn1Click(Sender: TObject);
var
  danfse1: TACBrNFSeXDANFSeFPDF;
begin
  OpenDialog1.Title := 'Selecione a NFSe';
  OpenDialog1.DefaultExt := '*-NFSe.xml';
  OpenDialog1.Filter :=
    'Arquivos NFSe (*-NFSe.xml)|*-NFSe.xml|Arquivos XML (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*';
  OpenDialog1.InitialDir := ACBrNFSeX1.Configuracoes.Arquivos.PathSalvar;

  if not OpenDialog1.Execute then
    Exit;

  //ConfigurarComponente
  ACBrNFSeX1.Configuracoes.WebServices.UF := 'SP';
  ACBrNFSeX1.Configuracoes.Geral.CodigoMunicipio := 3511508; //CerquilhoSP
//  ACBrNFSeX1.Configuracoes.Geral.Emitente.
  //...fim

  ACBrNFSeX1.NotasFiscais.Clear;

  // LoadFromFile - Usado para carregar o Xml de apenas uma nota
  ACBrNFSeX1.NotasFiscais.LoadFromFile(OpenDialog1.FileName, False);
  danfse1:= TACBrNFSeXDANFSeFPDF.Create(ACBrNFSeX1);
  try
  //??
//    ACBrNFSeX1.NotasFiscais[0].NFSe.ConstrucaoCivil.CodigoObra := '1234';
//    ACBrNFSeX1.NotasFiscais[0].NFSe.ConstrucaoCivil.Art := 'ART987';

    danfse1.Logo := 'D:\Componentes\ACBr\Fontes\Imagens\Logotipos\NFS-e_ACBr_peq.jpg';
    danfse1.Prefeitura := 'PREFEITURA DO MUNICIPIO DE CERQUILHO '+ #10+
                          'SECRETARIA MUNICIPAL DE FINANÇAS'+ #10+
                          'NOTA FISCAL ELETRÔNICA DE SERVIÇOS - NFe';
    danfse1.OutrasInformacaoesImp := 'PREFEITURA DO MUNICIPIO DE CERQUILHO SECRETARIA MUNICIPAL DE FINANÇAS 0123456789 0123456789 0123456789 0123456789 0123456789 0123456789 0123456789 0123456789'+
                          'NOTA FISCAL ELETRÔNICA DE SERVIÇOS - NFe 0123456789 0123456789 0123456789 0123456789 0123456789 0123456789 0123456789';

    ACBrNFSeX1.DANFSE := danfse1;
    ACBrNFSeX1.NotasFiscais.ImprimirPDF;

  finally
    danfse1.Free;
  end;


end;

end.
