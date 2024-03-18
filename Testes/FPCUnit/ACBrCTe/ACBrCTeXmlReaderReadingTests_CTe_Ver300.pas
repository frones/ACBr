unit ACBrCTeXmlReaderReadingTests_CTe_Ver300;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ACBrCTeXmlHandler, ACBrTests.Util,
  pcteCTe, ACBrCTeTestConsts;

type

  { TACBrCTeXmlReaderReadingTests_CTeComplementar_Ver300 }

  TACBrCTeXmlReaderReadingTests_CTeComplementar_Ver300 = class(TTestCase)
    published
      procedure LerXML_LeuCorretamenteOsDadosDoGrupo_infCTeComp;
  end;

implementation

{ TACBrCTeXmlReaderReadingTests_CTeComplementar_Ver300 }

procedure TACBrCTeXmlReaderReadingTests_CTeComplementar_Ver300.LerXML_LeuCorretamenteOsDadosDoGrupo_infCTeComp;
var
  FCTe: TCTe;
  FCTeReader: TCTeXmlReader;
begin
  FCTe := TCTe.Create;
  FCTeReader := TCTeXmlReader.Create(FCTe);
  try
     FCTeReader.CarregarArquivo(XML_CTE_COMPLEMENTAR_VERSAO300);
     FCTeReader.LerXml;

     //É a mesma estrutura do XML comum, por isso vou testar somente o grupo especifico
     AssertEquals('CTe.infCTeComp.chave', '35231118760500000000570010000000011946904021' , FCTe.infCteComp.chave)

  finally
    FCTeReader.Free;
    FCTe.Free;
  end;
end;

initialization
  _RegisterTest('ACBrCTeXmlReader.TestesLeitura.Complementar', TACBrCTeXmlReaderReadingTests_CTeComplementar_Ver300);

end.

