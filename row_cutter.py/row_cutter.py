import numpy as np
import PIL.Image as Image
import pdf2image
import sys, os, glob
from tqdm import tqdm

def get_files(path, ext='pdf'):
    rChar = '/' if sys.platform == 'posix' else '\\'
    return [f.split(rChar)[-1] for f in glob.glob(path + '/*.' + ext.lower())]

def pdf_to_images(pdf_path):
    pages = pdf2image.convert_from_path(pdf_path) #, dpi=300, fmt='png')
    # for pg in range(len(pages)):
    #     pages[pg].save('out_imgs/' + str(pg) + '.png')
    return [pg for pg in pages]

def save_np_arr_as_img(np_arr, img_path):
    Image.fromarray(np.uint8(np_arr)).save(img_path)
    return True

def iterate_over_rows(img_np_arr):
    gRows = [row for row in range(img_np_arr.shape[0]-1) if (np.array_equal(img_np_arr[row], img_np_arr[row+1]))]
    gCols = [col for col in range(img_np_arr.shape[1]-1) if (np.array_equal(img_np_arr[:, col], img_np_arr[:, col+1]))]
    new_arr = np.delete(img_np_arr, gRows, axis=0)
    new_arr = np.delete(new_arr, gCols, axis=1)
    return new_arr

def convert(pdfs_path='./pdf_files', out_path='./out_imgs'):
    if not os.path.exists(out_path):
        os.makedirs(out_path)
    
    for pdfFile in tqdm(get_files(pdfs_path, 'pdf')):
        outFile = out_path + '/' + pdfFile[:-4] + '.png'
        images = pdf_to_images(pdfs_path + '/' + pdfFile)
        for img in tqdm(range(len(images))):
            save_np_arr_as_img(iterate_over_rows(np.array(images[img])), outFile+'-'+str(img)+'.png')
    return True

convert('./pdf_files', './out_imgs')