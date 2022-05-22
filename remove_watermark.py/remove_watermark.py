from PyPDF2 import PdfFileReader, PdfFileWriter
import os
import glob
import sys


def removeWatermark(input_fname, output_fname):
    with open(input_fname, "rb") as inputFile, open(output_fname, "wb") as outputFile:
        reader = PdfFileReader(inputFile)
        writer = PdfFileWriter()

        for n in range(reader.numPages):
            page = reader.getPage(n)
            del page["/Contents"][-1]
            writer.addPage(page)
        writer.write(outputFile)
 
def main():
    pdfs = glob.glob('./input_folder/*.pdf')
    os.makedirs('./output_folder', exist_ok=True)
    for pdf in pdfs:
        try:
            removeWatermark(pdf, './output_folder/' + os.path.basename(pdf))
            print('Processed ' + pdf)
        except:
            print ("Error removing watermark from " + pdf)
            pass
    print('Finished.')
    input('Press Enter to exit...')

if __name__ == "__main__":
    main()

