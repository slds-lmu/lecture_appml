import argparse
import nbformat
import subprocess


def add_solution_tag(notebook_path, solution_marker, solution_tag='remove_cell'):
    """
    Process the notebook:
      - For code cells, look for '#' + solution_marker.
      - For markdown cells, look for solution_marker.
    If found, add the solution_tag to the cell's metadata.
    First clears any existing solution tags, then re-adds them based on current content.
    """
    with open(notebook_path, 'r', encoding='utf-8') as f:
        nb = nbformat.read(f, as_version=4)

    # First pass: clear any existing solution tags
    for cell in nb.cells:
        if 'tags' in cell.metadata and solution_tag in cell.metadata['tags']:
            cell.metadata['tags'].remove(solution_tag)
            # Remove empty tags list to keep metadata clean
            if not cell.metadata['tags']:
                del cell.metadata['tags']

    # Second pass: add solution tags where the marker is found
    for cell in nb.cells:
        if cell.cell_type == 'code':
            marker_text = '#' + solution_marker
        elif cell.cell_type == 'markdown':
            marker_text = solution_marker
        else:
            continue

        if marker_text in cell.source:
            tags = cell.metadata.get('tags', [])
            if solution_tag not in tags:
                tags.append(solution_tag)
                cell.metadata['tags'] = tags

    # Write the file back with updated tags
    with open(notebook_path, 'w', encoding='utf-8') as f:
        nbformat.write(nb, f)


def export_notebook(notebook_path, remove_solutions):
    """
    Export the notebook to HTML. If remove_solutions is True,
    enable the TagRemovePreprocessor to remove cells tagged with 'remove_cell'.
    """
    cmd = ['jupyter', 'nbconvert', '--to', 'html', notebook_path]
    if remove_solutions:
        cmd.extend([
            '--TagRemovePreprocessor.enabled=True',
            "--TagRemovePreprocessor.remove_cell_tags=['remove_cell']"
        ])

    subprocess.run(cmd, check=True)


def main():
    parser = argparse.ArgumentParser(
        description="Export a Jupyter Notebook to HTML with optional removal of solution cells."
    )
    parser.add_argument(
        'notebook',
        help="Path to the Jupyter Notebook (.ipynb)"
    )
    parser.add_argument(
        '--marker',
        default='===SOLUTION===',
        help=("Marker text to identify solution cells. For code cells, "
              "the script looks for '#' + marker (e.g., '#===SOLUTION==='); "
              "for markdown cells, it looks for the marker as is (e.g., '===SOLUTION===').")
    )
    parser.add_argument(
        '--remove',
        action='store_true',
        help="Remove solution cells in the exported HTML (using nbconvert TagRemovePreprocessor)."
    )

    args = parser.parse_args()

    # Update notebook: add metadata tags to cells containing the solution marker.
    add_solution_tag(args.notebook, args.marker)

    # Export the notebook to HTML.
    export_notebook(args.notebook, args.remove)

    print("Export completed successfully.")


if __name__ == '__main__':
    main()
